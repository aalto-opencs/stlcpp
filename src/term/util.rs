// These utilities are primarily used for testing

use num_bigint::BigInt;

use crate::r#type::{named_type::NamedType, util::arrow};

pub use super::Term::{self, *};

pub fn var(name: impl ToString) -> Term {
    Var(name.to_string())
}

// Implicitly converts strings to variable terms
impl From<&str> for Box<Term> {
    fn from(var: &str) -> Self {
        Box::new(Var(var.to_string()))
    }
}

// Implicitly converts integers to integer terms
impl From<BigInt> for Box<Term> {
    fn from(int: BigInt) -> Self {
        Box::new(Int(int))
    }
}

pub fn abs(var: impl ToString, ty: impl Into<NamedType>, body: impl Into<Box<Term>>) -> Term {
    Abs {
        var: var.to_string(),
        ty: ty.into(),
        body: body.into(),
    }
}
pub fn app(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    App(t1.into(), t2.into())
}
pub fn letin(var: impl ToString, val_t: impl Into<Box<Term>>, body: impl Into<Box<Term>>) -> Term {
    Let {
        var: var.to_string(),
        val_t: val_t.into(),
        body: body.into(),
    }
}

pub fn ite(
    cond: impl Into<Box<Term>>,
    if_true: impl Into<Box<Term>>,
    if_false: impl Into<Box<Term>>,
) -> Term {
    Ite {
        cond: cond.into(),
        if_true: if_true.into(),
        if_false: if_false.into(),
    }
}

pub fn add(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Add(t1.into(), t2.into())
}
pub fn sub(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Sub(t1.into(), t2.into())
}
pub fn mul(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Mul(t1.into(), t2.into())
}
pub fn eq(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Eq(t1.into(), t2.into())
}
pub fn ne(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Ne(t1.into(), t2.into())
}
pub fn lt(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Lt(t1.into(), t2.into())
}
pub fn le(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Le(t1.into(), t2.into())
}
pub fn gt(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Gt(t1.into(), t2.into())
}
pub fn ge(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Ge(t1.into(), t2.into())
}

pub fn pair(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
    Pair(t1.into(), t2.into())
}
pub fn fst(t: impl Into<Box<Term>>) -> Term {
    Fst(t.into())
}
pub fn snd(t: impl Into<Box<Term>>) -> Term {
    Snd(t.into())
}

pub fn nil(ty: impl Into<NamedType>) -> Term {
    Nil(ty.into())
}
pub fn cons(head: impl Into<Box<Term>>, tail: impl Into<Box<Term>>) -> Term {
    Cons(head.into(), tail.into())
}
pub fn lcase(
    t: impl Into<Box<Term>>,
    nil_t: impl Into<Box<Term>>,
    head_var: impl ToString,
    tail_var: impl ToString,
    cons_t: impl Into<Box<Term>>,
) -> Term {
    LCase {
        t: t.into(),
        nil_t: nil_t.into(),
        head_var: head_var.to_string(),
        tail_var: tail_var.to_string(),
        cons_t: cons_t.into(),
    }
}

pub fn inl(t: impl Into<Box<Term>>, ty_r: impl Into<NamedType>) -> Term {
    Inl(t.into(), ty_r.into())
}
pub fn inr(t: impl Into<Box<Term>>, ty_r: impl Into<NamedType>) -> Term {
    Inr(t.into(), ty_r.into())
}

pub fn case(
    t: impl Into<Box<Term>>,
    inl_var: impl ToString,
    inl_t: impl Into<Box<Term>>,
    inr_var: impl ToString,
    inr_t: impl Into<Box<Term>>,
) -> Term {
    Case {
        t: t.into(),
        inl_var: inl_var.to_string(),
        inl_t: inl_t.into(),
        inr_var: inr_var.to_string(),
        inr_t: inr_t.into(),
    }
}

pub fn fix(t: impl Into<Box<Term>>) -> Term {
    Fix(t.into())
}

pub fn id2() -> Term {
    abs("x", NamedType::Boolean, "x")
}
pub fn id22() -> Term {
    abs("x", arrow(NamedType::Boolean, NamedType::Boolean), "x")
}
pub fn tapp(t1: impl Into<Box<Term>>, t2: impl Into<NamedType>) -> Term {
    TApp(t1.into(), t2.into())
}
pub fn tabs(var: impl ToString, body: impl Into<Box<Term>>) -> Term {
    TAbs {
        var: var.to_string(),
        body: body.into(),
    }
}
