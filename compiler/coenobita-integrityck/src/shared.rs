use rustc_span::ErrorGuaranteed;

use coenobita_middle::flow::FlowPair;
use coenobita_middle::ty::Ty as Ty_;

pub type Ty = Ty_<FlowPair>;
pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
