#![feature(rustc_private)]

extern crate rustc_driver;

use std::{
    collections::HashMap,
    fs::{self, OpenOptions},
    io::{Read, Write},
    path::Path,
};

use coenobita_middle::{
    flow::FlowPair,
    origin::OriginSet,
    provenance::{self, ProvenancePair},
    ty::{Ty, TyKind},
};
use serde::{Deserialize, Serialize};

#[macro_export]
macro_rules! set {
    // Empty hashset - requires type annotation when used
    () => {
        ::std::collections::HashSet::new()
    };

    // Single element
    ($element:expr) => {
        {
            let mut set = ::std::collections::HashSet::with_capacity(1);
            set.insert($element);
            set
        }
    };

    // Multiple elements
    ($($element:expr),+ $(,)?) => {
        {
            let elements = [$($element),+];
            let mut set = ::std::collections::HashSet::with_capacity(elements.len());
            for element in elements {
                set.insert(element);
            }
            set
        }
    };
}

#[macro_export]
macro_rules! map {
    {} => {
        ::std::collections::HashMap::new()
    };

    { $($key:expr => $value:expr),+ $(,)? } => {
        ::std::collections::HashMap::from_iter([$(($key, $value)),+])
    };
}

#[derive(Debug, Serialize, Deserialize)]
struct TyData {
    provenance: Ty<ProvenancePair>,
    integrity: Ty<FlowPair>,
}

fn main() {
    let mut struct_map = HashMap::new();

    // Add an entry for `coenobita::Capability`
    let any = OriginSet::Universal;
    let root = OriginSet::Specific(set!["root".to_owned()]);

    let ty_kind_provenance = TyKind::Adt(map![
        "0".to_string() => Ty::new(ProvenancePair::new(any.clone(), root.clone()), TyKind::Opaque)
    ]);

    let provenance = Ty::new(ProvenancePair::new(any.clone(), any.clone()), ty_kind_provenance);

    struct_map.insert("coenobita::Capability", provenance);

    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("output")
        .with_extension("json");

    let output = serde_json::to_string(&struct_map).unwrap();

    println!("{:?}", path);

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&path)
        .unwrap();

    let res = file.write(output.as_bytes());

    println!("{:?}", res);

    // Now let's get it back
    let map_str = fs::read_to_string(path).unwrap();

    let map: HashMap<String, Ty<ProvenancePair>> = serde_json::from_str(&map_str).unwrap();

    println!("Reading the information back:");
    println!("{:#?}", map);
}
