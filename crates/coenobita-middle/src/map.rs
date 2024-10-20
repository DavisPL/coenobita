use std::collections::HashMap;

pub struct Map<F, T> {
    map: HashMap<F, T>,
}

impl<F, T> Map<F, T> {
    pub fn new() -> Self {
        todo!()
    }

    pub fn get(&self, key: &F) -> Option<&T> {
        todo!()
    }
}
