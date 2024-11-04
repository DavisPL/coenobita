#![feature(rustc_private)]

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

use std::fs::{File, OpenOptions};
use std::io::{self, Write};
use std::path::Path;
use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;

static LOGGER: Lazy<Logger> = Lazy::new(|| {
    Logger::new("/Users/georgeberdovskiy/Desktop/UCD/Research/PLDI25/coenobita/coenobita.log").unwrap()
});

pub struct Logger {
    file: Arc<Mutex<File>>,
}

impl Logger {
    pub fn new<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let file = OpenOptions::new().create(true).append(true).open(path)?;
        Ok(Logger {
            file: Arc::new(Mutex::new(file)),
        })
    }

    pub fn debug<T: AsRef<str>>(&self, msg: T) {
        let msg = format!("[DEBUG] {}\n", msg.as_ref());
        let _ = self.file.lock().unwrap().write_all(msg.as_bytes());
    }
}

pub fn debug<T: AsRef<str>>(msg: T) {
    LOGGER.debug(msg);
}
