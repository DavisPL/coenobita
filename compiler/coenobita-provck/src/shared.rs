use rustc_span::ErrorGuaranteed;

use coenobita_middle::provenance::ProvenancePair;
use coenobita_middle::ty::Ty as Ty_;

pub type Ty = Ty_<ProvenancePair>;
pub type Result<T = ()> = std::result::Result<T, ErrorGuaranteed>;
