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
        let mem = if let Some((mem, _)) = module.memories.entries().last() {
            mem
        } else {
            module.memories.push(waffle::MemoryData {
                initial_pages: 64,
                maximum_pages: None,
                segments: vec![],
            })
        };
        module.exports.push(waffle::Export {
            name: "0".to_string(),
            kind: waffle::ExportKind::Memory(mem),
        });
        Self {
            module,
            memory: mem,
            alloc_top: 0,
        }
    }

    pub fn function_with_sig(
        &mut self, signature: waffle::Signature, name: Option<impl Into<String>>
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
}
