use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::Itertools;
use log::debug;
use serde::{Deserialize, Serialize};

use crate::set::{Set, SetBind, SetUnion};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Integrity(pub Set, pub Set, pub Set);

impl Integrity {
    pub fn influence(&self, other: &Integrity) -> Self {
        let mut influenced = other.clone();

        // debug!(
        //     "influencers will be {:?} U {:?}",
        //     self.influencers(),
        //     other.influencers()
        // );

        let influencers = SetUnion::new(vec![self.influencers(), other.influencers()]);

        // debug!("influencers are now {:?}", influencers);

        influenced.2 = Set::Union(influencers);
        influenced
    }

    pub fn influencers(&self) -> &Set {
        &self.2
    }
}

impl Integrity {
    pub fn top() -> Self {
        let top = Set::Concrete(None);
        Self(top.clone(), top.clone(), top)
    }

    pub fn bottom(origin: String) -> Self {
        let mut origins = HashSet::new();
        origins.insert(origin);

        let bottom = Set::Concrete(Some(origins));
        Self(bottom.clone(), bottom.clone(), bottom)
    }
}

impl Display for Integrity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{},{})", self.0, self.1, self.2)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Type {
    pub integrity: Integrity,
    pub form: TypeForm,
}

impl Type {
    pub fn new(integrity: Integrity, form: TypeForm) -> Self {
        Self { integrity, form }
    }

    pub fn influence(&self, other: &Type) -> Self {
        let mut influenced = other.clone();
        influenced.integrity = self.integrity.influence(&other.integrity);
        influenced
    }

    pub fn form(&self) -> TypeForm {
        self.form.clone()
    }

    pub fn type_fn(n: usize) -> Self {
        let integrity = Integrity::top();
        let default = Type::new(integrity.clone(), TypeForm::Infer);

        let args = vec![default.clone(); n];
        let form = TypeForm::Fn(vec![], args, Box::new(default));

        Type { integrity, form }
    }

    pub fn ty_adt(n: usize) -> Type {
        // let default_flow_pair = FlowPair::new(OriginSet::Universal, OriginSet::Universal);
        // let default_ty = Type::new(default_flow_pair.clone(), TypeForm::Infer);

        // let args = vec![default_ty.clone(); n]; // Create `n` copies of `default_ty`
        // let mut map = HashMap::new();

        // for (i, arg) in (0..n).zip(args) {
        //     map.insert(i.to_string(), arg);
        // }

        // Type {
        //     property: default_flow_pair,
        //     form: TypeForm::Adt(map),
        // }
        todo!()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.form, self.integrity)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeForm {
    Opaque,
    Fn(Vec<SetBind>, Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Adt(HashMap<String, Type>),
    Infer,
}

impl Display for TypeForm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(_, arg_tys, ret_ty) => {
                let args = arg_tys.iter().map(|ty| ty.to_string()).sorted().join(",");

                write!(f, " fn ({args}) -> {}", ret_ty)
            }

            Self::Tuple(item_tys) => {
                let items = item_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                write!(f, " ({})", items)
            }

            Self::Array(item_ty) => write!(f, " [{}]", item_ty),

            Self::Adt(field_tys) => {
                let fields = field_tys
                    .iter()
                    .sorted_by_key(|&(key, _)| key)
                    .map(|(key, value)| format!("{key}:{value}"))
                    .collect::<Vec<_>>()
                    .join(",");

                write!(f, " struct {{{fields}}}")
            }

            Self::Opaque => write!(f, ""),
            Self::Infer => write!(f, ""),
        }
    }
}
