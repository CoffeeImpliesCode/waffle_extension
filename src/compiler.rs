use crate::expr::*;
use crate::function_builder::*;
use crate::intrinsics::Intrinsic;
use crate::module_builder::*;
use crate::types::*;
use anyhow::Result;

#[derive(Debug, Clone)]
pub enum Function {
    Import(waffle::Func),
    Intrinsic(std::rc::Rc<dyn Intrinsic>),
    User(waffle::Func),
}

pub type FunctionTable = std::collections::HashMap<String, Function>;

pub enum Statement {
    Expr(waffle::Func),
    Def(String, waffle::Func),
}

pub fn compile_statement<'a>(
    expr: &Expr,
    name: &str,
    mb: &'a mut ModuleBuilder,
    tab: &FunctionTable,
) -> Result<waffle::Func> {
    // println!("compile_statement: {:?}", expr);
    let ret = get_return_type(expr, tab, &mb.module);

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

pub fn get_return_type(
    expr: &Expr,
    table: &FunctionTable,
    module: &waffle::Module,
) -> Vec<waffle::Type> {
    match expr {
        Expr::Literal(l) => match l {
            LiteralExpr::Int(_) => vec![waffle::Type::I32],
            LiteralExpr::Str(_) => vec![waffle::Type::I32, waffle::Type::I32],
            _ => unimplemented!(),
        },
        Expr::FunctionCall(fc) => get_return_type(&*fc.function, table, module),
        Expr::Identifier(i) => match &table[&*i] {
            Function::Import(f) | Function::User(f) => module.signatures[module.funcs[*f].sig()].returns.clone(),
            Function::Intrinsic(i) => i.ret(),
        },
        Expr::If(IfExpr {conditional, consequent
        , alternate}) => {
            get_return_type(&consequent, table, module)
        }
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
    match expr {
        Expr::Literal(l) => compile_literal(l, fb, tab),
        Expr::FunctionCall(fc) => {
            let iden = match &*fc.function {
                Expr::Identifier(i) => i,
                _ => unimplemented!(),
            };

            match &tab[&*iden] {
                Function::Import(f) | Function::User(f) => {
                    let args = fc
                        .arguments
                        .iter()
                        .map(|e| compile_expr(e, fb, tab))
                        .flatten()
                        .collect::<Vec<_>>();
                    vec![fb.build_call(*f, args.as_slice())]
                }
                Function::Intrinsic(intr) => {
                    let args = fc
                        .arguments
                        .iter()
                        .map(|e| compile_expr(e, fb, tab))
                        .flatten()
                        .collect::<Vec<_>>();
                    vec![intr.call(fb, args.as_slice())]
                }
            }
        }
        Expr::If(IfExpr {conditional, consequent, alternate}) => {
            let cond = compile_expr(conditional, fb, tab);
            let consequent_block = fb.add_block(&[]);
            let alternative_block = fb.add_block(&[]);
            let typ = get_return_type(consequent.as_ref(), tab, &fb.mb.module);
            let result_block = fb.add_block(&typ);
            fb.build_condbr(cond[0], (consequent_block, &[]), (alternative_block, &[]));
            fb.focus_block(consequent_block);
            let consequent = compile_expr(consequent, fb, tab);
            fb.build_br(result_block, &consequent);
            fb.focus_block(alternative_block);
            let alternate = compile_expr(alternate, fb, tab);
            fb.build_br(result_block, &alternate);
            fb.focus_block(result_block);
            fb.body.blocks[fb.current_block()].params.iter().map(|(_, v)| *v).collect::<Vec<_>>()
        }
        _ => unimplemented!("compile_expr: {:?}", expr),
    }
}

pub fn compile_literal(
    l: &LiteralExpr,
    fb: &mut FunctionBuilder,
    tab: &FunctionTable,
) -> Vec<waffle::Value> {
    match l {
        LiteralExpr::Int(i) => vec![fb.build_i32(*i as i32)],
        LiteralExpr::Str(s) => {
            let text = s.replace("\\n", "\n");
            let (mem, addr, len) = fb.mb.alloc(text.as_bytes());
            let addr = fb.build_i32(addr as i32);
            let len = fb.build_i32(len as i32);
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
