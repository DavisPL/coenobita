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

use log::{info, LevelFilter};
use rustc_driver::run_compiler;
use std::{
    env,
    fs::{self, OpenOptions},
    path::Path,
};

use callbacks::CoenobitaCallbacks;

use simplelog::{ConfigBuilder, WriteLogger};

fn main() {
    // Collect all the arguments passed to us by Cargo
    let mut args: Vec<String> = env::args().skip(1).collect();

    let crate_type = crate_type(&args).unwrap_or("-".into());

    let crate_name = crate_name(&args).unwrap_or("-".into());

    // Set up logging
    logging_setup(&crate_name);

    info!("CRATE TYPE: {crate_type}");
    info!("CRATE NAME (ACTUAL): {crate_name}");

    let crate_name = if crate_type == "root" {
        "root".to_owned()
    } else {
        crate_name
    };

    info!("CRATE NAME (ANALYSIS): {crate_name}");

    // Add some extra arguments
    args.push("-Zcrate-attr=feature(register_tool)".to_string());
    args.push("-Zcrate-attr=register_tool(cnbt)".to_string());
    args.push("-Zcrate-attr=register_tool(coenobita)".to_string());

    // Create callbacks and run the compiler
    let mut callbacks = CoenobitaCallbacks::new(crate_name, crate_type);
    run_compiler(&args, &mut callbacks);
}

fn crate_name<'a>(args: &'a [String]) -> Option<String> {
    for arg in args {
        if let Some(name) = arg.strip_prefix("--crate-name=") {
            return Some(name.to_owned());
        }
    }

    for pair in args.windows(2) {
        if pair[0] == "--crate-name" {
            return Some(pair[1].clone());
        }
    }

    None
}

fn crate_type(args: &[String]) -> Option<String> {
    let crate_name = crate_name(args);

    if matches!(crate_name.as_deref(), Some("build_script_build")) || args.iter().any(|arg| arg == "--test") {
        return Some("lib".to_owned());
    }

    if matches!(crate_name.as_deref(), Some("std")) {
        return Some("root".to_owned());
    }

    for (i, arg) in args.iter().enumerate() {
        let value = if arg == "--crate-type" {
            args.get(i + 1).map(String::as_str)
        } else {
            arg.strip_prefix("--crate-type=")
        };

        if let Some(value) = value {
            if value.split(',').any(|kind| kind == "bin") {
                return Some("root".to_owned());
            }

            return Some("lib".to_owned());
        }
    }

    if env::var_os("CARGO_BIN_NAME").is_some() || args.iter().any(|arg| arg.ends_with("/src/main.rs") || arg == "src/main.rs") {
        return Some("root".to_owned());
    }

    None
}

#[allow(unused)]
fn crate_release(args: &[String]) -> bool {
    return args.contains(&"--release".to_owned());
}

fn logging_setup(crate_name: &str) {
    // Check the environment variable
    let log_level = env::var("COENOBITA_LOG_LEVEL").unwrap_or_else(|_| "DEBUG".to_string());

    // Decide the level filter based on the environment variable
    let level_filter = match log_level.as_str() {
        "DEBUG" => LevelFilter::Debug,
        "INFO" => LevelFilter::Info,
        "WARN" => LevelFilter::Warn,
        "ERROR" => LevelFilter::Error,
        _ => LevelFilter::Off,
    };

    // If logging is set to off, skip initialization
    if level_filter == LevelFilter::Off {
        return;
    }

    // Determine the root directory
    let root_dir = env!("CARGO_MANIFEST_DIR");

    // Create logging directory if it doesn't already exist
    let log_dir = Path::new(root_dir).join("../..").join("logs");
    fs::create_dir_all(&log_dir).expect(&format!(
        "Failed to create Coenobita logging directory at {}",
        log_dir.display()
    ));

    let path = log_dir.join(crate_name).with_extension("log");
    let log = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&path)
        .expect(&format!(
            "Failed to create Whelk logging file at {}",
            path.display()
        ));

    let config = ConfigBuilder::new()
        .set_time_level(log::LevelFilter::Off) // Don't show time
        .set_target_level(log::LevelFilter::Off) // Don't show target (like "(1)")
        .set_thread_level(log::LevelFilter::Off) // Don't show thread ID
        .set_location_level(log::LevelFilter::Off) // Don't show file/line location
        .build();

    WriteLogger::init(simplelog::LevelFilter::Debug, config, log)
        .expect("Could not initialize Coenobita logger");
}
