use crate::r#type::named_type::NamedType::{self, *};

pub fn abs(var: impl ToString, body: impl Into<Box<NamedType>>) -> NamedType {
    Abs(var.to_string(), body.into())
}

pub fn arrow(ty1: impl Into<Box<NamedType>>, ty2: impl Into<Box<NamedType>>) -> NamedType {
    Arrow(ty1.into(), ty2.into())
}
pub fn prod(ty1: impl Into<Box<NamedType>>, ty2: impl Into<Box<NamedType>>) -> NamedType {
    Prod(ty1.into(), ty2.into())
}
pub fn list(ty: impl Into<Box<NamedType>>) -> NamedType {
    List(ty.into())
}
pub fn sum(ty1: impl Into<Box<NamedType>>, ty2: impl Into<Box<NamedType>>) -> NamedType {
    Sum(ty1.into(), ty2.into())
}

// Implicitly converts strings to variable types
impl From<usize> for NamedType {
    fn from(var: usize) -> Self {
        Var(var)
    }
}
impl From<usize> for Box<NamedType> {
    fn from(var: usize) -> Self {
        Box::new(Var(var))
    }
}
pub fn tvar(name: usize) -> NamedType {
    Var(name)
}
