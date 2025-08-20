//! These utilities are distinct from the ones given to the student, as we don't test the utils in the submission.
#![allow(unused_imports)]
#![allow(dead_code)]

pub use stlcpp::module::*;
pub use stlcpp::term::Term::{self, *};
pub use stlcpp::term::util::*;
pub use stlcpp::r#type::TypeError::{self, *};
pub use stlcpp::r#type::named_type::NamedType::{self, *};

// pub fn var(var: impl ToString) -> Term {
//     Var(var.to_string())
// }
// pub fn abs(var: impl ToString, ty: impl Into<Type>, body: impl Into<Box<Term>>) -> Term {
//     Abs {
//         var: var.to_string(),
//         ty: ty.into(),
//         body: body.into(),
//     }
// }
// pub fn app(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     App(t1.into(), t2.into())
// }
// pub fn letin(var: impl ToString, val_t: impl Into<Box<Term>>, body: impl Into<Box<Term>>) -> Term {
//     Let {
//         var: var.to_string(),
//         val_t: val_t.into(),
//         body: body.into(),
//     }
// }

// pub fn ite(
//     cond: impl Into<Box<Term>>,
//     if_true: impl Into<Box<Term>>,
//     if_false: impl Into<Box<Term>>,
// ) -> Term {
//     Ite {
//         cond: cond.into(),
//         if_true: if_true.into(),
//         if_false: if_false.into(),
//     }
// }

// pub fn add(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Add(t1.into(), t2.into())
// }
// pub fn sub(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Sub(t1.into(), t2.into())
// }
// pub fn mul(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Mul(t1.into(), t2.into())
// }
// pub fn eq(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Eq(t1.into(), t2.into())
// }
// pub fn ne(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Ne(t1.into(), t2.into())
// }
// pub fn lt(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Lt(t1.into(), t2.into())
// }
// pub fn le(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Le(t1.into(), t2.into())
// }
// pub fn gt(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Gt(t1.into(), t2.into())
// }
// pub fn ge(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Ge(t1.into(), t2.into())
// }

// pub fn pair(t1: impl Into<Box<Term>>, t2: impl Into<Box<Term>>) -> Term {
//     Pair(t1.into(), t2.into())
// }
// pub fn fst(t: impl Into<Box<Term>>) -> Term {
//     Fst(t.into())
// }
// pub fn snd(t: impl Into<Box<Term>>) -> Term {
//     Snd(t.into())
// }

// pub fn nil(ty: impl Into<Type>) -> Term {
//     Nil(ty.into())
// }
// pub fn cons(head: impl Into<Box<Term>>, tail: impl Into<Box<Term>>) -> Term {
//     Cons(head.into(), tail.into())
// }
// pub fn lcase(
//     t: impl Into<Box<Term>>,
//     nil_t: impl Into<Box<Term>>,
//     head_var: impl ToString,
//     tail_var: impl ToString,
//     cons_t: impl Into<Box<Term>>,
// ) -> Term {
//     LCase {
//         t: t.into(),
//         nil_t: nil_t.into(),
//         head_var: head_var.to_string(),
//         tail_var: tail_var.to_string(),
//         cons_t: cons_t.into(),
//     }
// }

// pub fn inl(t: impl Into<Box<Term>>, ty_r: impl Into<Type>) -> Term {
//     Inl(t.into(), ty_r.into())
// }
// pub fn inr(t: impl Into<Box<Term>>, ty_r: impl Into<Type>) -> Term {
//     Inr(t.into(), ty_r.into())
// }

// pub fn case(
//     t: impl Into<Box<Term>>,
//     inl_var: impl ToString,
//     inl_t: impl Into<Box<Term>>,
//     inr_var: impl ToString,
//     inr_t: impl Into<Box<Term>>,
// ) -> Term {
//     Case {
//         t: t.into(),
//         inl_var: inl_var.to_string(),
//         inl_t: inl_t.into(),
//         inr_var: inr_var.to_string(),
//         inr_t: inr_t.into(),
//     }
// }

// pub fn fix(t: impl Into<Box<Term>>) -> Term {
//     Fix(t.into())
// }

pub fn id2(var: impl ToString) -> Term {
    abs(var.to_string(), Boolean, var.to_string().as_str())
}
pub fn id2_() -> Term {
    id2("x")
}
pub fn tru() -> Term {
    abs("x", Boolean, abs("y", Boolean, var("x")))
}
pub fn fals() -> Term {
    abs("x", Boolean, abs("y", Boolean, var("y")))
}
