use waffle::Func;

use crate::function_builder::FunctionBuilder;
pub trait Intrinsic: std::fmt::Debug {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value;
    fn args(&self) -> Vec<waffle::Type>;
    fn ret(&self) -> Vec<waffle::Type>;
}

#[derive(Default, Debug)]
pub struct AddI32;
impl Intrinsic for AddI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        fb.build_addi32(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32, waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct SubI32;
impl Intrinsic for SubI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        fb.build_subi32(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32, waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct MulI32;
impl Intrinsic for MulI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        fb.build_muli32(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32, waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct DivI32;
impl Intrinsic for DivI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        fb.build_divi32(args[0], args[1])
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32, waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct EqI32;
impl Intrinsic for EqI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 2);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        assert_eq!(
            fb.body.values[args[1]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );

        match args {
            [] => unreachable!(),
            [a] => unreachable!(),
            [a, b] => fb.build_eqi32(*a, *b),
            _ => unreachable!(),
        }
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32, waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct AddI64;
impl Intrinsic for AddI64 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
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

#[derive(Default, Debug)]
pub struct SubI64;
impl Intrinsic for SubI64 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
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

#[derive(Default, Debug)]
pub struct MulI64;
impl Intrinsic for MulI64 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
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

#[derive(Default, Debug)]
pub struct DivI64;
impl Intrinsic for DivI64 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
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

#[derive(Default, Debug)]
pub struct StackPushI32 {
    pub sp: waffle::Global,
}
impl Intrinsic for StackPushI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 1);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        let val = args[0];
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let dec = fb.build_i32(1);
        let new_sp = fb.build_subi32(sp, dec);
        fb.build_storei32(new_sp, val, 0);
        fb.build_global_set(self.sp, new_sp)
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![]
    }
}

#[derive(Default, Debug)]
pub struct StackPopI32 {
    pub sp: waffle::Global,
}
impl Intrinsic for StackPopI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 0);
        // assert_eq!(
        //     fb.body.values[args[0]].ty(&fb.body.type_pool),
        //     Some(waffle::Type::I32)
        // );
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let val = fb.build_loadi32(sp, 0);
        let dec = fb.build_i32(1);
        let new_sp = fb.build_addi32(sp, dec);
        fb.build_global_set(self.sp, new_sp);
        val
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct StackPushFrame {
    pub sp: waffle::Global,
    pub bp: waffle::Global,
}
impl Intrinsic for StackPushFrame {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 0);
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let bp = fb.build_global_get(self.bp, waffle::Type::I32);
        fb.build_call_intrinsic(&StackPushI32 { sp: self.sp }, &[bp]);

        fb.build_global_set(self.bp, sp)
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![]
    }
}

#[derive(Default, Debug)]
pub struct StackPopFrame {
    pub sp: waffle::Global,
    pub bp: waffle::Global,
}
impl Intrinsic for StackPopFrame {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 0);
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let bp = fb.build_global_get(self.bp, waffle::Type::I32);
        fb.build_global_set(self.sp, bp);
        let new_bp = fb.build_call_intrinsic(&StackPopI32 { sp: self.sp }, &[]);
        fb.build_global_set(self.bp, new_bp)
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![]
    }
}

#[derive(Default, Debug)]
pub struct StackAllocI32 {
    pub sp: waffle::Global,
    pub n: i32,
}
impl Intrinsic for StackAllocI32 {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 0);
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let dec = fb.build_i32(self.n);
        let new_sp = fb.build_subi32(sp, dec);
        fb.build_global_set(self.sp, new_sp);
        new_sp
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}

#[derive(Default, Debug)]
pub struct StackAllocI32V {
    pub sp: waffle::Global,
}
impl Intrinsic for StackAllocI32V {
    fn call(&self, fb: &mut FunctionBuilder, args: &[waffle::Value]) -> waffle::Value {
        assert_eq!(args.len(), 1);
        assert_eq!(
            fb.body.values[args[0]].ty(&fb.body.type_pool),
            Some(waffle::Type::I32)
        );
        let sp = fb.build_global_get(self.sp, waffle::Type::I32);
        let dec = args[0];
        let new_sp = fb.build_subi32(sp, dec);
        fb.build_global_set(self.sp, new_sp);
        new_sp
    }

    fn args(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }

    fn ret(&self) -> Vec<waffle::Type> {
        vec![waffle::Type::I32]
    }
}
