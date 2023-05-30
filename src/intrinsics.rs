use crate::function_builder::FunctionBuilder;
pub trait Intrinsic {
    fn call(&self, args: &[waffle::Value], fb: &mut FunctionBuilder) -> waffle::Value;
    fn args(&self) -> Vec<waffle::Type>;
    fn ret(&self) -> Vec<waffle::Type>;
}

#[derive(Default)]
pub struct AddI64;
impl Intrinsic for AddI64 {
    fn call(&self, args: &[waffle::Value], fb: &mut FunctionBuilder) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        fb.build_addi64(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64, waffle::Type::I64]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64]
    }
}

#[derive(Default)]
pub struct SubI64;
impl Intrinsic for SubI64 {
    fn call(&self, args: &[waffle::Value], fb: &mut FunctionBuilder) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        fb.build_subi64(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64, waffle::Type::I64]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64]
    }
}

#[derive(Default)]
pub struct MulI64;
impl Intrinsic for MulI64 {
    fn call(&self, args: &[waffle::Value], fb: &mut FunctionBuilder) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        fb.build_muli64(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64, waffle::Type::I64]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64]
    }
}

#[derive(Default)]
pub struct DivI64;
impl Intrinsic for DivI64 {
    fn call(&self, args: &[waffle::Value], fb: &mut FunctionBuilder) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I64)
        );
        fb.build_divi64(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64, waffle::Type::I64]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I64]
    }
}
