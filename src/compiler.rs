use crate::expr::*;
use crate::function_builder::*;
use crate::intrinsics::Intrinsic;
use crate::module_builder::*;
use crate::types::*;
use anyhow::Result;

pub enum ExternFunction {
    Import(waffle::Func, waffle::SignatureData),
    Intrinsic(Box<dyn Intrinsic>),
}

pub type FunctionTable = std::collections::HashMap<String, ExternFunction>;

pub fn compile_statement<'a>(
    expr: &Expr,
    name: &str,
    mb: &'a mut ModuleBuilder,
    tab: &FunctionTable,
) -> Result<waffle::Func> {
    // println!("compile_statement: {:?}", expr);
    let ret = get_return_type(expr, tab);

    // let sig_data = waffle::SignatureData {
    //     params: vec![],
    //     returns: ret,
    // };

    // let sig = mb.module.signatures.push(sig_data);

    let b = {
        let mut fb = mb.function(vec![], ret, Some(name));
        // let mut fb = FunctionBuilder::new(mb, sig, Some(name));
        let v = compile_expr(expr, &mut fb, tab);
        // let ty = fb.body.values[v].tys(&mut fb.body.type_pool).unwrap();

        fb.build_return(&v);
        let b = fb.build();
        b
    };
    Ok(mb.module.funcs.push(b))
}

pub fn get_return_type(expr: &Expr, table: &FunctionTable) -> Vec<waffle::Type> {
    match expr {
        Expr::Literal(l) => match l {
            LiteralExpr::Int(_) => vec![waffle::Type::I32],
            LiteralExpr::Str(_) => vec![waffle::Type::I32, waffle::Type::I32],
            _ => unimplemented!(),
        },
        Expr::FunctionCall(fc) => get_return_type(&*fc.function, table),
        Expr::Identifier(i) => match &table[&*i] {
            ExternFunction::Import(func, sig) => sig.returns.clone(),
            ExternFunction::Intrinsic(i) => i.ret(),
        },
        _ => {
            unimplemented!()
        }
    }
}

pub fn compile_expr(
    expr: &Expr,
    fb: &mut FunctionBuilder,
    tab: &FunctionTable,
) -> Vec<waffle::Value> {
    // println!("compile_expr: {:?}", expr);
    match expr {
        Expr::Literal(l) => compile_literal(l, fb, tab),
        Expr::FunctionCall(fc) => {
            let iden = match &*fc.function {
                Expr::Identifier(i) => i,
                _ => unimplemented!(),
            };

            match &tab[&*iden] {
                ExternFunction::Import(f, sig) => {
                    let args = fc
                        .arguments
                        .iter()
                        .map(|e| compile_expr(e, fb, tab))
                        .flatten()
                        .collect::<Vec<_>>();
                    vec![fb.build_call(*f, sig.clone(), args.as_slice())]
                }
                ExternFunction::Intrinsic(intr) => {
                    let args = fc
                        .arguments
                        .iter()
                        .map(|e| compile_expr(e, fb, tab))
                        .flatten()
                        .collect::<Vec<_>>();
                    vec![intr.call(args.as_slice(), fb)]
                }
            }
        }
        _ => unimplemented!(),
    }
}

pub fn compile_literal(
    l: &LiteralExpr,
    fb: &mut FunctionBuilder,
    tab: &FunctionTable,
) -> Vec<waffle::Value> {
    // println!("compile_literal: {:?}", l);
    match l {
        LiteralExpr::Int(i) => vec![fb.build_i64(*i)],
        LiteralExpr::Str(s) => {
            let (mem, addr, len) = fb.mb.alloc(s.as_bytes());
            let addr = fb.build_i32(addr as i32);
            let len = fb.build_i32(len as i32);
            // fb.build_join(&[addr, len])
            vec![addr, len]
        }
        l_ => unimplemented!(),
    }
}

// pub fn compile_test(module: &mut waffle::Module, tab: &mut Table) -> Result<waffle::Func> {
//     let sig_data = waffle::SignatureData {
//         params: vec![waffle::Type::I32, waffle::Type::I32],
//         returns: vec![waffle::Type::I32],
//     };

//     let sig = module.signatures.push(sig_data);

//     let mut f = FunctionBuilder::new(&module, sig, Some("foo"));

//     let a0 = f.build_arg0();
//     let a1 = f.build_arg1();
//     // let add = f.build_addi32(a0, a1);
//     let (test, sig_data) = tab.get("test").unwrap().clone();
//     let call = f.build_call(test, sig_data, &[a0, a1]);
//     let r2 = f.build_output_select(call, 0, waffle::Type::I32);
//     f.build_return(&[r2]);

//     let foo = module.funcs.push(f.build());

//     Ok(foo)
// }
