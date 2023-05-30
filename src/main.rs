#![allow(unused)]
mod compiler;
mod expr;
mod function_builder;
mod intrinsics;
mod module_builder;
mod parser;
mod types;

use anyhow::Result;
use compiler::*;
use function_builder::FunctionBuilder;
use intrinsics::*;
use module_builder::ModuleBuilder;

use rustyline::{error::ReadlineError, DefaultEditor};

pub fn test(a: i64, b: i64) -> i64 {
    a - b + 1
}

pub fn _add_i64(a: i64, b: i64) -> i64 {
    a + b
}

pub fn _sub_i64(a: i64, b: i64) -> i64 {
    a - b
}

type Mem = wasmer::FunctionEnv<Option<wasmer::Memory>>;
type MemMut<'a> = wasmer::FunctionEnvMut<'a, Option<wasmer::Memory>>;

pub fn copy_mem(mut env: MemMut, addr: u64, len: usize) -> Vec<u8> {
    let (mem, store) = env.data_and_store_mut();
    let mut data = vec![0; len];
    let res = mem
        .as_mut()
        .map(|mem| mem.view(&store).read(addr, &mut data))
        .unwrap();
    data
}

pub fn display(mut env: MemMut, a: i32, b: i32) -> i64 {
    unsafe {
        let res = copy_mem(env, a as u64, b as usize);
        println!("{}", std::str::from_utf8_unchecked(&res));
        b as i64
    }
}

fn declare_intrinsic<I: Intrinsic + 'static>(name: &str, intrinsic: I, table: &mut FunctionTable) {
    // let sig = add_signature(module, params, returns);
    // let func = module.funcs.push(waffle::FuncDecl::Local(sig));
    table.insert(name.into(), ExternFunction::Intrinsic(Box::new(intrinsic)));
}

fn declare_import(
    module: &mut waffle::Module,
    name: &str,
    params: Vec<waffle::Type>,
    returns: Vec<waffle::Type>,
    table: &mut FunctionTable,
) -> waffle::Func {
    let sig = add_signature(module, params, returns);
    let func = add_import(module, name, sig);
    table.insert(
        name.into(),
        ExternFunction::Import(func, module.signatures[sig].clone()),
    );
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

fn add_import(module: &mut waffle::Module, name: &str, sig: waffle::Signature) -> waffle::Func {
    let f = module
        .funcs
        .push(waffle::FuncDecl::Import(sig, name.into()));
    module.imports.push(waffle::Import {
        module: "env".into(),
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

    declare_import(
        &mut module,
        "test",
        vec![waffle::Type::I64, waffle::Type::I64],
        vec![waffle::Type::I64],
        &mut table,
    );

    declare_import(
        &mut module,
        "display",
        vec![waffle::Type::I32, waffle::Type::I32],
        vec![waffle::Type::I64],
        &mut table,
    );

    declare_intrinsic("+", AddI64, &mut table);
    declare_intrinsic("-", SubI64, &mut table);
    declare_intrinsic("*", MulI64, &mut table);
    declare_intrinsic("/", DivI64, &mut table);

    (module, table)
}

fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;

    let mut i = 0;

    let (module, table) = module_and_table();

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

        println!("{}", mb.module.display());
        run(&mb.module, &*name)?;
    }
}

fn run(module: &waffle::Module, name: &str) -> Result<()> {
    let wasm_bytes = module.to_wasm_bytes()?;
    waffle::wasmparser::validate(&wasm_bytes)?;
    let wat = wabt::wasm2wat(&wasm_bytes)?;
    println!("{wat}");

    let mut store = wasmer::Store::default();

    let module = wasmer::Module::new(&store, wasm_bytes)?;

    let mem_ptr = Mem::new(&mut store, None);

    let import_object = wasmer::imports! {
        "env" => {
            "test" => wasmer::Function::new_typed(&mut store, test),
            "display" => wasmer::Function::new_typed_with_env(&mut store, &mem_ptr, display),
        },
    };

    let instance = wasmer::Instance::new(&mut store, &module, &import_object)?;

    *mem_ptr.as_mut(&mut store) = Some(instance.exports.get_memory("0").unwrap().clone());

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
    repl()
}
