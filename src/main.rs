#![feature(rustc_private)]

mod callbacks;
mod utilities;

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

use std::env;

use callbacks::CoenobitaCallbacks;
use utilities::debug;

use rustc_driver::RunCompiler;

fn main() {
    // Collect all the arguments passed to us by Cargo
    let mut args: Vec<String> = env::args().skip(1).collect();

    // Add some extra arguments
    args.push("-Zcrate-attr=feature(register_tool)".to_string());
    args.push("-Zcrate-attr=register_tool(cnbt)".to_string());

    let crate_name = crate_name(&args)
        .and_then(|s| {
            debug(format!("Lagoon invoked for crate '{s}'\n========="));
            Some(s)
        })
        .unwrap_or("-");

    // Create callbacks and run the compiler
    let mut callbacks = CoenobitaCallbacks::new(crate_name);
    let _result = RunCompiler::new(&args, &mut callbacks).run();
}

fn crate_name<'a>(args: &'a [String]) -> Option<&'a str> {
    for pair in args.windows(2) {
        if pair[0] == "--crate-name" {
            return Some(&pair[1]);
        }
    }

    None
}
