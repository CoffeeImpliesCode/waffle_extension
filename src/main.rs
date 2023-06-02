#![allow(unused)]

mod compiler;
mod expr;
mod function_builder;
mod intrinsics;
mod module_builder;
mod parser;
mod types;

use std::vec;

use anyhow::Result;
use compiler::*;
use function_builder::FunctionBuilder;
use intrinsics::*;
use module_builder::ModuleBuilder;

use rustyline::{error::ReadlineError, DefaultEditor};
use wabt::Module;
use wasmer::{AsStoreRef, ModuleMiddleware};

type Mem = wasmer::FunctionEnv<Option<wasmer::Memory>>;
type MemMut<'a> = wasmer::FunctionEnvMut<'a, Option<wasmer::Memory>>;

pub fn copy_mem(mut env: MemMut, addr: u64, len: usize) -> Vec<u8> {
    /* wasmer 3.3.0 */
    // env
    // let (mem, store) = env.data_and_store_mut();
    // let mut data = vec![0; len];
    // let res = mem
    //     .as_mut()
    //     .map(|mem| mem.view(&store).read(addr, &mut data))
    //     .unwrap();
    // res

    /* wasmer 3.1.1 + wasmer_wasi */
    let mut data = vec![0; len];
    let mem = env.data().as_ref().unwrap();
    mem.view(&env.as_store_ref()).read(addr, &mut data);

    data
}

fn declare_intrinsic<I: Intrinsic + 'static>(name: &str, intrinsic: I, table: &mut FunctionTable) {
    // let sig = add_signature(module, params, returns);
    // let func = module.funcs.push(waffle::FuncDecl::Local(sig));
    table.insert(
        name.into(),
        Function::Intrinsic(std::rc::Rc::new(intrinsic)),
    );
}

fn declare_import(
    module: &mut waffle::Module,
    ns: &str,
    name: &str,
    params: Vec<waffle::Type>,
    returns: Vec<waffle::Type>,
    table: &mut FunctionTable,
) -> waffle::Func {
    let sig = add_signature(module, params, returns);
    let func = add_import(module, ns, name, sig);
    table.insert(name.into(), Function::Import(func));
    func
}

fn add_signature(
    module: &mut waffle::Module,
    params: Vec<waffle::Type>,
    returns: Vec<waffle::Type>,
) -> waffle::Signature {
    let sig = module
        .signatures
        .push(waffle::SignatureData { params, returns });
    sig
}

fn add_import(
    module: &mut waffle::Module,
    ns: &str,
    name: &str,
    sig: waffle::Signature,
) -> waffle::Func {
    let f = module
        .funcs
        .push(waffle::FuncDecl::Import(sig, name.into()));
    module.imports.push(waffle::Import {
        module: ns.into(),
        name: name.into(),
        kind: waffle::ImportKind::Func(f),
    });
    f
}

fn module_and_table() -> (waffle::Module<'static>, FunctionTable) {
    let mut module = waffle::Module {
        orig_bytes: &[],
        funcs: waffle::entity::EntityVec::default(),
        signatures: waffle::entity::EntityVec::default(),
        globals: waffle::entity::EntityVec::default(),
        tables: waffle::entity::EntityVec::default(),
        imports: vec![],
        exports: vec![],
        memories: waffle::entity::EntityVec::default(),
        start_func: None,
        debug: waffle::Debug::default(),
        debug_map: waffle::DebugMap::default(),
    };

    let mut table = std::collections::HashMap::new();

    // declare_import(
    //     &mut module,
    //     "env",
    //     "test",
    //     vec![waffle::Type::I32, waffle::Type::I32],
    //     vec![waffle::Type::I32],
    //     &mut table,
    // );

    // declare_import(
    //     &mut module,
    //     "env",
    //     "display",
    //     vec![waffle::Type::I32, waffle::Type::I32],
    //     vec![waffle::Type::I32],
    //     &mut table,
    // );

    let fd_write = declare_import(
        &mut module,
        "wasi_snapshot_preview1",
        "fd_write",
        vec![
            waffle::Type::I32,
            waffle::Type::I32,
            waffle::Type::I32,
            waffle::Type::I32,
        ],
        vec![waffle::Type::I32],
        &mut table,
    );

    let mut mb: ModuleBuilder = module.into();

    let sp = mb.global(waffle::Type::I32, Some((2u64).pow(10)), true);
    let bp = mb.global(waffle::Type::I32, Some((2u64).pow(10)), true);

    let display = build_display(&mut mb, sp, bp, fd_write);
    let displayln = build_displayln(&mut mb, sp, bp, fd_write);
    let len = build_len(&mut mb);
    table.insert("display".into(), Function::User(display));
    table.insert("displayln".into(), Function::User(displayln));
    table.insert("len".into(), Function::User(len));

    declare_intrinsic("+", AddI32, &mut table);
    declare_intrinsic("-", SubI32, &mut table);
    declare_intrinsic("*", MulI32, &mut table);
    declare_intrinsic("/", DivI32, &mut table);
    declare_intrinsic("push", StackPushI32 { sp }, &mut table);
    declare_intrinsic("==", EqI32, &mut table);

    let module = mb.into();

    (module, table)
}

fn build_display(
    mb: &mut ModuleBuilder,
    sp: waffle::Global,
    bp: waffle::Global,
    fd_write: waffle::Func,
) -> waffle::Func {
    let mut fb = mb.function(
        vec![waffle::Type::I32, waffle::Type::I32],
        vec![waffle::Type::I32],
        Some("display"),
    );

    let spv = fb.build_call_intrinsic(&StackPushFrame { sp, bp }, &[]);
    let iovecs_len = fb.build_i32(1);
    let iovecs_ptr = fb.build_call_intrinsic(&StackAllocI32V { sp }, &[iovecs_len]);
    let fd_write_ret_ptr = fb.build_call_intrinsic(&StackAllocI32 { sp, n: 1 }, &[]);

    let text_ptr = fb.build_arg0();
    let text_len = fb.build_arg1();

    fb.build_storei32(iovecs_ptr, text_ptr, 0);
    fb.build_storei32(iovecs_ptr, text_len, 4);

    let fd = fb.build_i32(1);
    let addr = fb.build_call(fd_write, &[fd, iovecs_ptr, iovecs_len, fd_write_ret_ptr]);
    let res = fb.build_loadi32(fd_write_ret_ptr, 0);

    let spv = fb.build_call_intrinsic(&StackPopFrame { sp, bp }, &[]);

    fb.build_return(&[res]);
    let b = fb.build();
    let f = mb.module.funcs.push(b);
    f
}

fn build_displayln(
    mb: &mut ModuleBuilder,
    sp: waffle::Global,
    bp: waffle::Global,
    fd_write: waffle::Func,
) -> waffle::Func {
    let mut fb = mb.function(
        vec![waffle::Type::I32, waffle::Type::I32],
        vec![waffle::Type::I32],
        Some("displayln"),
    );

    let spv = fb.build_call_intrinsic(&StackPushFrame { sp, bp }, &[]);
    let iovecs_len = fb.build_i32(2);
    let iovecs_ptr = fb.build_call_intrinsic(&StackAllocI32V { sp }, &[iovecs_len]);
    let fd_write_ret_ptr = fb.build_call_intrinsic(&StackAllocI32 { sp, n: 1 }, &[]);

    let text_ptr = fb.build_arg0();
    let text_len = fb.build_arg1();

    let nl_ptr = fb.build_call_intrinsic(&StackAllocI32 { sp, n: 1 }, &[]);
    let nl_len = fb.build_i32(1);
    let nl_val = fb.build_i32(b'\n' as i32);
    fb.build_storei32(nl_ptr, nl_val, 0);

    fb.build_storei32(iovecs_ptr, text_ptr, 0);
    fb.build_storei32(iovecs_ptr, text_len, 4);
    fb.build_storei32(iovecs_ptr, nl_ptr, 8);
    fb.build_storei32(iovecs_ptr, nl_len, 12);

    let fd = fb.build_i32(1);
    let addr = fb.build_call(fd_write, &[fd, iovecs_ptr, iovecs_len, fd_write_ret_ptr]);
    let res = fb.build_loadi32(fd_write_ret_ptr, 0);

    let spv = fb.build_call_intrinsic(&StackPopFrame { sp, bp }, &[]);

    fb.build_return(&[res]);
    let b = fb.build();
    let f = mb.module.funcs.push(b);
    f
}

fn build_len(mb: &mut ModuleBuilder) -> waffle::Func {
    let mut fb = mb.function(
        vec![waffle::Type::I32, waffle::Type::I32],
        vec![waffle::Type::I32],
        Some("len"),
    );
    let len = fb.build_arg1();
    fb.build_return(&[len]);
    let b = fb.build();
    let f = mb.module.funcs.push(b);
    f
}

fn build_readline(fb: &mut FunctionBuilder) -> waffle::Func {
    todo!()
}

fn repl(ssa: bool, wat: bool) -> Result<()> {
    let mut rl = DefaultEditor::new()?;

    let mut i = 0;

    let (module, mut table) = module_and_table();

    let mut mb = ModuleBuilder::new(module);

    loop {
        let line = rl.readline(">> ")?;
        let mut name = format!("#{i}");
        i += 1;
        let fun = { compile_statement(&crate::parser::parse(&*line)?, &name, &mut mb, &table)? };
        mb.module.exports.push(waffle::Export {
            name: name.clone(),
            kind: waffle::ExportKind::Func(fun),
        });
        table.insert(name.clone(), Function::User(fun));
        if ssa {
            println!("{}", mb.module.display());
        }
        run(&mb.module, &*name, wat)?;
    }
}

fn run(module: &waffle::Module, name: &str, wat: bool) -> Result<()> {
    let wasm_bytes = module.to_wasm_bytes()?;
    waffle::wasmparser::validate(&wasm_bytes)?;
    if wat {
        let wat = wabt::wasm2wat(&wasm_bytes)?;
        println!("{wat}");
    }
    let mut store = wasmer::Store::default();

    let module = wasmer::Module::new(&store, wasm_bytes)?;

    let mem_ptr = Mem::new(&mut store, None);

    let mut import_object = wasmer::imports! {
        "env" => {
            // "test" => wasmer::Function::new_typed(&mut store, test),
            // "display" => wasmer::Function::new_typed_with_env(&mut store, &mem_ptr, display),
        },
    };

    let wasi_state = wasmer_wasi::WasiState::new("femtolisp").build()?;

    let wasi_env = wasmer::FunctionEnv::<wasmer_wasi::WasiEnv>::new(
        &mut store,
        wasmer_wasi::WasiEnv::new(wasi_state),
    );

    let wasi = wasmer_wasi::generate_import_object_from_env(
        &mut store,
        &wasi_env,
        wasmer_wasi::WasiVersion::Snapshot1,
    );

    import_object.extend(&wasi);

    // for ((m, name), f) in &import_object {
    //     println!("{} {}: {:#?}", m, name, f.ty(&store));
    // }

    let instance = wasmer::Instance::new(&mut store, &module, &import_object)?;

    wasi_env
        .as_mut(&mut store)
        .set_memory(instance.exports.get_memory("memory").unwrap().clone());
    *mem_ptr.as_mut(&mut store) = Some(instance.exports.get_memory("memory").unwrap().clone());

    let f = instance.exports.get_function(name)?;

    let res = f.call(&mut store, &[])?;
    print!("-- ",);
    for r in &*res {
        print!(" {:?}", r);
    }
    println!();
    Ok(())
}

fn main() -> Result<()> {
    println!(
        r#"
    Welcome to Crêpe!
    Arguments:
        --ssa       Print SSA IR after each statement
        --wat       Print WAT after each statement
    Possible syntax:
        (+ 1 2)
        (* 3 (/ 4 2))
        (displayln "Hello, World!")
    "#
    );

    let mut ssa = false;
    let mut wat = false;

    let mut args = std::env::args();
    for arg in args.into_iter().skip(1) {
        if arg == "--ssa" {
            ssa = true;
        } else if arg == "--wat" {
            wat = true;
        } else {
            println!("Unknown argument: {}", arg);
            return Ok(());
        }
    }

    repl(ssa, wat)
}
