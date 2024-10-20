use std::fs::{File, OpenOptions};
use std::io::{self, Write};
use std::path::Path;
use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;
use rustc_data_structures::sync;
use rustc_errors::annotate_snippet_emitter_writer::AnnotateSnippetEmitter;
use rustc_errors::emitter::{stderr_destination, Emitter, HumanEmitter, HumanReadableErrorType};
use rustc_errors::json::JsonEmitter;
use rustc_errors::registry::Registry;
use rustc_errors::LazyFallbackBundle;
use rustc_session::config::{ErrorOutputType, Options};
use rustc_span::source_map::SourceMap;

static LOGGER: Lazy<Logger> = Lazy::new(|| {
    Logger::new("/Users/georgeberdovskiy/Desktop/UCD/Research/PLDI25/lagoon/log").unwrap()
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

pub fn emitter(
    opts: Options,
    source_map: Arc<SourceMap>,
    fallback_bundle: LazyFallbackBundle,
) -> Box<dyn Emitter + sync::DynSend> {
    let bundle = None;
    let track_diagnostics = opts.unstable_opts.track_diagnostics;

    match opts.error_format {
        ErrorOutputType::HumanReadable(err_type, color_config) => {
            if let HumanReadableErrorType::AnnotateSnippet = err_type {
                let emitter = AnnotateSnippetEmitter::new(
                    Some(source_map),
                    None,
                    fallback_bundle,
                    false,
                    false,
                );
                Box::new(emitter)
            } else {
                let dst = stderr_destination(color_config);
                let emitter = HumanEmitter::new(dst, fallback_bundle)
                    .sm(Some(source_map))
                    .short_message(err_type.short())
                    .diagnostic_width(opts.diagnostic_width)
                    .track_diagnostics(track_diagnostics)
                    .terminal_url(opts.unstable_opts.terminal_urls);
                Box::new(emitter)
            }
        }
        ErrorOutputType::Json {
            pretty,
            json_rendered,
            color_config,
        } => Box::new(
            JsonEmitter::new(
                Box::new(io::BufWriter::new(io::stderr())),
                source_map,
                fallback_bundle,
                pretty,
                json_rendered,
                color_config,
            )
            .registry(Some(Registry::new(&[])))
            .fluent_bundle(bundle)
            .track_diagnostics(track_diagnostics)
            .diagnostic_width(opts.diagnostic_width)
            .terminal_url(opts.unstable_opts.terminal_urls),
        ),
    }
}
