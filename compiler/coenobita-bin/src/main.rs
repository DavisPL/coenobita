#![feature(rustc_private)]

mod callbacks;

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_session;
extern crate rustc_span;

use log::{debug, error, info, warn};
use std::{env, fs::File, path::Path};

use callbacks::CoenobitaCallbacks;

use rustc_driver::RunCompiler;
use simplelog::{Config, ConfigBuilder, WriteLogger};

fn main() {
    // Collect all the arguments passed to us by Cargo
    let mut args: Vec<String> = env::args().skip(1).collect();

    // Add some extra arguments
    args.push("-Zcrate-attr=feature(register_tool)".to_string());
    args.push("-Zcrate-attr=register_tool(cnbt)".to_string());

    let crate_name = crate_name(&args)
        .and_then(|s| {
            debug!("Coenobita invoked for crate '{s}'\n=========");
            Some(s)
        })
        .unwrap_or("-".into());

    let crate_type = crate_type(&args)
        .and_then(|s| {
            debug!("Crate type is {s}");
            Some(s)
        })
        .unwrap_or("-".into());

    // Set up logging
    let log = File::create(format!(
        "/Users/georgeberdovskiy/Desktop/UCD/Research/PLDI25/coenobita/logs/{crate_name}.log"
    ))
    .expect("Could not create Coenobita logging file");

    let config = ConfigBuilder::new()
        .set_time_level(log::LevelFilter::Off) // Don't show time
        .set_target_level(log::LevelFilter::Off) // Don't show target (like "(1)")
        .set_thread_level(log::LevelFilter::Off) // Don't show thread ID
        .set_location_level(log::LevelFilter::Off) // Don't show file/line location
        .build();

    WriteLogger::init(simplelog::LevelFilter::Debug, config, log)
        .expect("Could not initialize Coenobita logger");

    if crate_name != "std" {
        args.push("--extern=std=/Users/georgeberdovskiy/Desktop/UCD/Research/PLDI25/coenobita/library/std/target/debug/libstd.rlib".to_string());
    }

    // Create callbacks and run the compiler
    let mut callbacks = CoenobitaCallbacks::new(crate_name, crate_type);
    let _result = RunCompiler::new(&args, &mut callbacks).run();
}

fn crate_name<'a>(args: &'a [String]) -> Option<String> {
    for pair in args.windows(2) {
        if pair[0] == "--crate-name" {
            return Some(pair[1].clone());
        }
    }

    None
}

fn crate_type(args: &[String]) -> Option<String> {
    args.windows(2)
        .find_map(|pair| match [pair[0].as_str(), &pair[1]] {
            ["--test", _] | [_, "--test"] => Some("root".to_owned()),
            ["--crate-name", "std"] => Some("root".to_owned()),
            ["--crate-type", "bin"] => Some("root".to_owned()),
            ["--crate-type", _] => Some("lib".to_owned()),
            _ => None,
        })
}
