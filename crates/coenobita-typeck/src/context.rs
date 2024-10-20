use coenobita_middle::ty::Ty;

pub struct Context<'cnbt> {
    crate_name: &'cnbt str,
}

impl<'cnbt> Context<'cnbt> {
    pub fn new(crate_name: &'cnbt str) -> Self {
        Context { crate_name }
    }

    pub fn default(&self) -> Ty {
        todo!()
    }

    pub fn influence(&self, ty: Ty) -> Ty {
        todo!()
    }

    pub fn enter(&mut self, ty: &Ty) {
        todo!()
    }

    pub fn exit(&mut self) {
        todo!()
    }
}
