use crate::function_builder::FunctionBuilder;

type Address = usize;
type Size = usize;
pub struct ModuleBuilder {
    pub module: waffle::Module<'static>,
    memory: waffle::Memory,
    alloc_top: usize,
}

impl ModuleBuilder {
    pub fn new(mut module: waffle::Module<'static>) -> Self {
        // if mem exists, find the top as our allocation base
        let mem = module.memories.entries().next().map(|(i, _)| i).to_owned();
        let (mem, alloc_top) = if let Some(mem) = mem {
            let alloc_top = module.memories[mem]
                .segments
                .iter()
                .map(|seg| (seg.offset, seg.data.len()))
                .max_by(|a, b| a.0.cmp(&b.0))
                .get_or_insert((0, 0)).clone();
            let alloc_top = alloc_top.0 + alloc_top.1;
            (mem, alloc_top)
        } else {
            // create new default memory
            let mem = module.memories.push(waffle::MemoryData {
                initial_pages: 64,
                maximum_pages: None,
                segments: vec![],
            });
            module.exports.push(waffle::Export {
                name: "memory".to_string(),
                kind: waffle::ExportKind::Memory(mem),
            });
            (mem, 0)
        };
        Self {
            module,
            memory: mem,
            alloc_top,
        }
    }

    pub fn function_with_sig(
        &mut self,
        signature: waffle::Signature,
        name: Option<impl Into<String>>,
    ) -> FunctionBuilder<'_> {
        FunctionBuilder::new(self, signature, name)
    }

    pub fn function(
        &mut self,
        params: Vec<waffle::Type>,
        returns: Vec<waffle::Type>,
        name: Option<impl Into<String>>,
    ) -> FunctionBuilder<'_> {
        let signature = self
            .module
            .signatures
            .push(waffle::SignatureData { params, returns });
        FunctionBuilder::new(self, signature, name)
    }

    pub fn declare_function(
        &mut self,
        params: Vec<waffle::Type>,
        returns: Vec<waffle::Type>,
        name: Option<impl Into<String>>,
    ) -> waffle::Func {
        let signature = self
            .module
            .signatures
            .push(waffle::SignatureData { params, returns });
        let f = self.module.funcs.push(waffle::FuncDecl::Body(
            signature,
            name.map(|s| s.into()).unwrap_or("".into()),
            waffle::FunctionBody::new(&self.module, signature),
        ));
        f
    }

    pub fn define_function(&mut self, f: waffle::Func) -> FunctionBuilder<'_> {
        let name = if self.module.funcs[f].name().is_empty() {
            None
        } else {
            Some(self.module.funcs[f].name().to_string())
        };
        FunctionBuilder::new(self, self.module.funcs[f].sig(), name)
    }

    pub fn get_function(&self, f: &str) -> Option<(waffle::Func, waffle::Signature)> {
        self.module
            .funcs
            .entries()
            .find(|(_, func)| func.name().ends_with(f))
            .map(|(i, delc)| (i, delc.sig()))
    }

    /// Allocates data in the module's memory initialized to `data`.
    /// Returns (memory, address, size in bytes)
    ///
    /// NOTE: memory is always entry zero as per https://webassembly.github.io/spec/core/syntax/modules.html#memories (footnote)
    pub fn alloc(&mut self, data: &[u8]) -> (waffle::Memory, Address, Size) {
        let mem0 = &mut self.module.memories[self.memory];
        let addr = self.alloc_top;
        self.alloc_top += data.len();
        mem0.segments.push(waffle::MemorySegment {
            offset: addr,
            data: data.to_vec(),
        });

        (self.memory, addr, data.len())
    }

    /// Preallocates data in the module's memory.
    /// Returns (memory, address, size in bytes)
    /// Like alloc, but does not initialize the memory.
    ///
    /// NOTE: memory is always entry zero as per https://webassembly.github.io/spec/core/syntax/modules.html#memories (footnote)
    pub fn prealloc(&mut self, len: usize) -> (waffle::Memory, Address, Size) {
        let mem0 = &mut self.module.memories[self.memory];
        let addr = self.alloc_top;
        self.alloc_top += len;

        (self.memory, addr, len)
    }

    pub fn global(&mut self, ty: waffle::Type, init: Option<u64>, mutable: bool) -> waffle::Global {
        self.module.globals.push(waffle::GlobalData {
            ty,
            value: init,
            mutable,
        })
    }
}

impl Into<ModuleBuilder> for waffle::Module<'static> {
    fn into(self) -> ModuleBuilder {
        ModuleBuilder::new(self)
    }
}

impl Into<waffle::Module<'static>> for ModuleBuilder {
    fn into(self) -> waffle::Module<'static> {
        self.module
    }
}
