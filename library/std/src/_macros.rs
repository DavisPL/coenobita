#[macro_export]
macro_rules! transmute {
    ($code:expr) => {
        unsafe { std::mem::transmute($code) }
    };
}
