use anyhow::*;

use crate::module_builder::ModuleBuilder;

pub struct FunctionBuilder<'a> {
    pub mb: &'a mut ModuleBuilder,
    pub signature: waffle::Signature,
    pub name: Option<String>,
    pub body: waffle::FunctionBody,
    pub current_block: waffle::Block,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(
        mb: &'a mut ModuleBuilder,
        // module: &waffle::Module,
        signature: waffle::Signature,
        name: Option<impl Into<String>>,
    ) -> Self {
        let name = name.map(|s| s.into());
        let body = waffle::FunctionBody::new(&mb.module, signature);
        let current_block = body.entry;
        Self {
            mb,
            signature,
            name,
            body,
            current_block,
        }
    }

    pub fn add_block(&mut self, params: &[waffle::Type]) -> waffle::Block {
        let block = self.body.add_block();
        for param in params {
            self.body.add_blockparam(block, *param);
        }
        block
    }

    pub fn current_block(&self) -> waffle::Block {
        self.current_block
    }

    pub fn current_block_params(&self) -> &[(waffle::Type, waffle::Value)] {
        &*self.body.blocks[self.current_block].params
    }

    pub fn current_block_param(&self, n: usize) -> (waffle::Type, waffle::Value) {
        self.body.blocks[self.current_block].params[n]
    }

    pub fn get_current_block_param(&self, n: usize) -> Option<(waffle::Type, waffle::Value)> {
        self.body.blocks[self.current_block].params.get(n).copied()
    }

    pub fn add_block_and_focus(&mut self, params: &[waffle::Type]) -> waffle::Block {
        let block = self.add_block(params);
        self.focus_block(block);
        block
    }

    /// Focus on a block, returning the previous block
    pub fn focus_block(&mut self, mut b: waffle::Block) -> waffle::Block {
        std::mem::swap(&mut self.current_block, &mut b);
        b
    }

    pub fn build_unary(
        &mut self,
        op: waffle::Operator,
        ret: waffle::Type,
        arg: waffle::Value,
    ) -> waffle::Value {
        let arg = self.body.arg_pool.single(arg);
        let ty = self.body.single_type_list(ret);
        let v = self.body.add_value(waffle::ValueDef::Operator(op, arg, ty));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_binary(
        &mut self,
        op: waffle::Operator,
        ret: waffle::Type,
        a: waffle::Value,
        b: waffle::Value,
    ) -> waffle::Value {
        let args = self.body.arg_pool.double(a, b);
        let ty = self.body.single_type_list(ret);

        let v = self
            .body
            .add_value(waffle::ValueDef::Operator(op, args, ty));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_ternary(
        &mut self,
        op: waffle::Operator,
        ret: waffle::Type,
        a: waffle::Value,
        b: waffle::Value,
        c: waffle::Value,
    ) -> waffle::Value {
        let args = self.body.arg_pool.triple(a, b, c);
        let ty = self.body.single_type_list(ret);

        let v = self
            .body
            .add_value(waffle::ValueDef::Operator(op, args, ty));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_addi32(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I32Add, waffle::Type::I32, a, b)
    }

    pub fn build_subi32(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I32Sub, waffle::Type::I32, a, b)
    }

    pub fn build_addi64(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I64Add, waffle::Type::I64, a, b)
    }

    pub fn build_subi64(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I64Sub, waffle::Type::I64, a, b)
    }

    pub fn build_muli64(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I64Mul, waffle::Type::I64, a, b)
    }

    pub fn build_divi64(&mut self, a: waffle::Value, b: waffle::Value) -> waffle::Value {
        self.build_binary(waffle::Operator::I64DivS, waffle::Type::I64, a, b)
    }

    pub fn build_arg0(&mut self) -> waffle::Value {
        self.body.blocks[self.body.entry].params[0].1
    }

    pub fn build_arg1(&mut self) -> waffle::Value {
        self.body.blocks[self.body.entry].params[1].1
    }

    pub fn append(&mut self, v: waffle::Value) -> waffle::Value {
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_arg(&mut self, n: usize) -> waffle::Value {
        self.append(self.body.blocks[self.body.entry].params[n].1)
    }

    pub fn build_i32(&mut self, i: i32) -> waffle::Value {
        self.build_const(
            waffle::Type::I32,
            waffle::Operator::I32Const { value: i as u32 },
        )
    }

    pub fn build_i64(&mut self, i: i64) -> waffle::Value {
        self.build_const(
            waffle::Type::I64,
            waffle::Operator::I64Const { value: i as u64 },
        )
    }

    pub fn build_f32(&mut self, f: f32) -> waffle::Value {
        self.build_const(
            waffle::Type::F32,
            waffle::Operator::F32Const { value: f.to_bits() },
        )
    }

    pub fn build_f64(&mut self, f: f64) -> waffle::Value {
        self.build_const(
            waffle::Type::F64,
            waffle::Operator::F64Const { value: f.to_bits() },
        )
    }

    pub fn build_call(
        &mut self,
        f: waffle::Func,
        sig: waffle::SignatureData,
        args: &[waffle::Value],
    ) -> waffle::Value {
        let argp_ref = self
            .body
            .arg_pool
            .allocate(args.len(), waffle::Value::default());
        let argp = &mut self.body.arg_pool[argp_ref];
        for (i, &input) in args.iter().enumerate() {
            argp[i] = args[i];
        }

        let tys = if sig.returns.len() == 1 {
            self.body.single_type_list(sig.returns[0])
        } else {
            self.body.type_pool.from_iter(sig.returns.iter().copied())
        };

        let v = self.body.add_value(waffle::ValueDef::Operator(
            waffle::Operator::Call { function_index: f },
            argp_ref,
            tys,
        ));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_output_select(
        &mut self,
        output: waffle::Value,
        index: u32,
        ty: waffle::Type,
    ) -> waffle::Value {
        let v = self
            .body
            .add_value(waffle::ValueDef::PickOutput(output, index, ty));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_const(&mut self, t: waffle::Type, op: waffle::Operator) -> waffle::Value {
        let ty = self.body.single_type_list(t);
        let v = self.body.add_value(waffle::ValueDef::Operator(
            op,
            waffle::pool::ListRef::default(),
            ty,
        ));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_join(&mut self, values: &[waffle::Value]) -> waffle::Value {
        let argp_ref = self
            .body
            .arg_pool
            .allocate(values.len(), waffle::Value::default());
        let argp = &mut self.body.arg_pool[argp_ref];
        for (i, &input) in values.iter().enumerate() {
            argp[i] = values[i];
        }
        let types = values
            .iter()
            .map(|v| self.body.values[*v].ty(&self.body.type_pool).unwrap())
            .collect::<Vec<_>>();
        let tys = self.body.type_pool.from_iter(types.iter().cloned());
        let v = self.body.add_value(waffle::ValueDef::Operator(
            waffle::Operator::Nop,
            argp_ref,
            tys,
        ));
        self.body.append_to_block(self.current_block, v);
        v
    }

    pub fn build_return(&mut self, values: &[waffle::Value]) {
        self.body.set_terminator(
            self.current_block,
            waffle::Terminator::Return {
                values: values.into(),
            },
        );
    }

    pub fn build(mut self) -> waffle::FuncDecl<'static> {
        self.body.recompute_edges();
        waffle::FuncDecl::Body(self.signature, self.name.unwrap_or("".into()), self.body)
    }
}
