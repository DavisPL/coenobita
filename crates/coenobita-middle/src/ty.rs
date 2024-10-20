use crate::flow::FlowPair;
use coenobita_ast::ast;

#[derive(Debug, Clone)]
pub struct Ty(TyKind);

#[derive(Debug, Clone)]
pub enum TyKind {
    Abs(FlowPair),
    Fn(FlowPair, Vec<Ty>, Box<Ty>),
    Tup(Vec<Ty>),
    Infer(FlowPair),
}

impl Ty {
    pub fn merge(self, other: Ty) -> Ty {
        todo!()
    }

    pub fn kind(self) -> TyKind {
        self.0
    }

    pub fn ty_fn(n: usize) -> Ty {
        todo!()
    }
}

impl From<ast::Ty> for Ty {
    fn from(value: ast::Ty) -> Self {
        Ty(value.into())
    }
}

impl From<ast::Ty> for TyKind {
    fn from(value: ast::Ty) -> Self {
        match value.kind {
            ast::TyKind::Abstract => Self::Abs(value.flow_pair.into()),
            ast::TyKind::Fn(arg_tys, ret_ty) => Self::Fn(
                value.flow_pair.into(),
                arg_tys.into_iter().map(|ty| ty.into()).collect(),
                Box::new(Ty::from(*ret_ty)),
            ),
            ast::TyKind::Tup(item_tys) => {
                Self::Tup(item_tys.into_iter().map(|ty| ty.into()).collect())
            }
        }
    }
}
