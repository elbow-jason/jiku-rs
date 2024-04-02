// Check. TypeExtensions cannot have descriptions. (Or should we handle that?)

use crate::{Api, QueryDoc, SchemaDoc};

pub trait SchemaPhase {
    type Error;
    type Output;
    fn check<'a>(&self, schema_doc: &SchemaDoc<'a>) -> Result<Self::Output, Self::Error>;
}

pub trait QueryPhase {
    type Error;
    type Output;
    fn check<'a>(&self, query_doc: &QueryDoc<'a>) -> Result<Self::Output, Self::Error>;
}

pub trait ApiPhase {
    type Error;
    type Output;
    fn check<'a>(&self, api: &Api<'a>) -> Result<Self::Output, Self::Error>;
}

pub trait ExePhase {
    type Error;
    type Output;
    fn check<'a>(
        &self,
        api: &Api<'a>,
        query_doc: &QueryDoc<'a>,
    ) -> Result<Self::Output, Self::Error>;
}
