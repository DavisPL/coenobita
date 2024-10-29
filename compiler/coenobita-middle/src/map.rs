use std::{collections::HashMap, hash::Hash};

pub struct Map<F, T> {
    map: HashMap<F, T>,
}

impl<F: Eq + Hash, T> Map<F, T> {
    pub fn new() -> Self {
        Map {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, key: &F) -> Option<&T> {
        self.map.get(key)
    }

    pub fn insert(&mut self, key: F, value: T) {
        self.map.insert(key, value);
    }
}
