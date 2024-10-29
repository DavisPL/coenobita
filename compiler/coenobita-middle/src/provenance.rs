use std::fmt::Display;

use coenobita_ast::provenance;

#[derive(Debug, Clone, PartialEq)]
pub struct ProvenancePair(pub Provenance, pub Provenance);

impl ProvenancePair {
    pub fn first(&self) -> &Provenance {
        &self.0
    }

    pub fn last(&self) -> &Provenance {
        &self.1
    }
}

impl From<provenance::ProvenancePair> for ProvenancePair {
    fn from(value: provenance::ProvenancePair) -> Self {
        ProvenancePair(value.first.into(), value.last.into())
    }
}

impl Display for ProvenancePair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.first(), self.last())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Provenance {
    Specific(String),
    Universal,
}

impl From<provenance::Provenance> for Provenance {
    fn from(value: provenance::Provenance) -> Self {
        match value {
            provenance::Provenance::Universal(_) => Self::Universal,
            provenance::Provenance::Specific(origin, _) => Self::Specific(origin.to_string()),
        }
    }
}

impl Display for Provenance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(origin) => write!(f, "{origin}"),
            Self::Universal => write!(f, "*"),
        }
    }
}
