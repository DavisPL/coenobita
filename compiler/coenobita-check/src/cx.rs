use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use coenobita_middle::set::Set;
use coenobita_middle::ty::Type;

pub struct Ctx<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K: Hash + Eq, V> Ctx<K, V> {
    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.scopes.pop();
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.scopes.iter().rev().find_map(|cx| cx.get(k))
    }

    pub fn set(&mut self, k: K, v: V) {
        match self.scopes.last_mut() {
            Some(cx) => cx.insert(k, v),
            None => None,
        };
    }
}

pub struct InfCtx {
    scopes: Vec<Set>,
}

impl InfCtx {
    pub fn new(origin: String) -> Self {
        let set = Set::Concrete(BTreeSet::from([origin]));

        InfCtx { scopes: vec![set] }
    }

    pub fn enter(&mut self, set: &Set) {
        self.scopes.push(set.clone());
    }

    pub fn exit(&mut self) {
        self.scopes.pop();
    }

    pub fn influence(&self, mut ty: Type) -> Type {
        for set in &self.scopes {
            ty.intrinsic[2] = ty.intrinsic[2].clone().union(set.clone());
        }

        ty
    }
}
