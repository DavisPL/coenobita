use crate::flow::FlowPair;

#[derive(Debug, Clone)]
pub struct Ty(TyKind);

#[derive(Debug, Clone)]
pub enum TyKind {
    Abs(FlowPair),
    Fn(FlowPair, Vec<Ty>, Box<Ty>),
    Infer(FlowPair),
}

impl Ty {
    pub fn merge(self, other: Ty) -> Ty {
        todo!()
    }

    pub fn kind(self) -> TyKind {
        self.0
    }
}
