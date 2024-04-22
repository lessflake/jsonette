// TODO:
// * i don't think references will work but cba testing yet
// * camel/default attrs

pub use jsonette_derive::*;

mod ser;
pub use ser::*;

mod de;
pub use de::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value<'a>(pub &'a [u8]);
