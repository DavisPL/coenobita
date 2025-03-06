use rustc_span::ErrorGuaranteed;

pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
