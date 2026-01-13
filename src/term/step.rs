use std::collections::HashMap;

use crate::r#type::named_type::NamedType;
use crate::stlcpp_stderr;

use super::Term::{self, *};

fn step_op1(
    ctor: impl FnOnce(Box<Term>) -> Term,
    eval: impl FnOnce(Box<Term>) -> Term,
    t: Box<Term>,
    env: &HashMap<String, Term>,
) -> Term {
    if t.is_err() {
        *t
    } else if t.is_value() {
        eval(t)
    } else {
        ctor(Box::new(t.step(env)))
    }
}

fn step_op2(
    ctor: impl FnOnce(Box<Term>, Box<Term>) -> Term,
    eval: impl FnOnce(Box<Term>, Box<Term>) -> Term,
    t1: Box<Term>,
    t2: Box<Term>,
    env: &HashMap<String, Term>,
) -> Term {
    if t1.is_err() {
        *t1
    } else if t2.is_err() {
        *t2
    } else {
        match (t1.is_value(), t2.is_value()) {
            (false, _) => ctor(Box::new(t1.step(env)), t2),
            (true, false) => ctor(t1, Box::new(t2.step(env))),
            (true, true) => eval(t1, t2),
        }
    }
}

fn eval_app(t1: Box<Term>, t2: Box<Term>) -> Term {
    if let Abs { var, ty: _, body } = *t1 {
        body.subst(&var, *t2)
    } else if let Compose(f, g) = *t1 {
        App(f, Box::new(App(g, t2)))
    } else {
        panic!("BUG: attempted to apply non abstraction to a value");
    }
}

fn eval_let(val_t: Box<Term>, var: impl AsRef<str>, body: Box<Term>) -> Term {
    body.subst(var.as_ref(), *val_t)
}

fn eval_comp(_: Box<Term>, _: Box<Term>) -> Term {
    panic!("BUG: attempted to step a composition")
}

fn eval_ite(cond: Box<Term>, if_true: Box<Term>, if_false: Box<Term>) -> Term {
    match *cond {
        True => *if_true,
        False => *if_false,
        _ => panic!("BUG: attempted to if-then-else with non boolean condition"),
    }
}

fn eval_add(t1: Box<Term>, t2: Box<Term>) -> Term {
    if let (Int(n1), Int(n2)) = (t1.as_ref(), t2.as_ref()) {
        Int(n1 + n2)
    } else {
        panic!("BUG: attempted to add non integers")
    }
}

fn eval_sub(t1: Box<Term>, t2: Box<Term>) -> Term {
    if let (Int(n1), Int(n2)) = (t1.as_ref(), t2.as_ref()) {
        Int(n1 - n2)
    } else {
        panic!("BUG: attempted to sub non integers")
    }
}

fn eval_mul(t1: Box<Term>, t2: Box<Term>) -> Term {
    if let (Int(n1), Int(n2)) = (t1.as_ref(), t2.as_ref()) {
        Int(n1 * n2)
    } else {
        panic!("BUG: attempted to mul non integers")
    }
}

fn eval_div(t1: Box<Term>, t2: Box<Term>) -> Term {
    if let (Int(n1), Int(n2)) = (t1.as_ref(), t2.as_ref()) {
        use num_bigint::BigInt;
        if *n2 == BigInt::from(0) {
            stlcpp_stderr!("*** STLC++ PANIC: division by zero");
            Panic(true, NamedType::Integer, Box::new(Int(n1.clone())))
        } else {
            Int(n1 / n2)
        }
    } else {
        panic!("BUG: attempted to div non integers")
    }
}

macro_rules! eval_cmp {
    ($oper:tt) => {
        |t1: Box<Term>, t2: Box<Term>| {
            match (t1.as_ref(), t2.as_ref()) {
                (Int(t1), Int(t2)) => {
                    if t1 $oper t2 {
                        True
                    } else {
                        False
                    }
                }
                (Char(t1), Char(t2)) => {
                    if t1 $oper t2 {
                        True
                    } else {
                        False
                    }
                }
                (t1 @ (True | False), t2 @ (True | False)) => {
                    if matches!(t1, True) $oper matches!(t2, True) {
                        True
                    } else {
                        False
                    }
                }
                _ => panic!("BUG: attempted to compare incompatible types"),
            }
        }
    }
}

fn eval_pair(_: Box<Term>, _: Box<Term>) -> Term {
    panic!("BUG: attempted to step pair of values")
}

fn eval_fst(t: Box<Term>) -> Term {
    let Pair(fst, _) = *t else {
        panic!("BUG: attempted to call fst on non pair value");
    };

    *fst
}

fn eval_snd(t: Box<Term>) -> Term {
    let Pair(_, snd) = *t else {
        panic!("BUG: attempted to call snd on non pair value");
    };

    *snd
}

fn eval_lcase(
    t: Box<Term>,
    nil_t: Box<Term>,
    head_var: &str,
    tail_var: &str,
    cons_t: Box<Term>,
) -> Term {
    match *t {
        Nil(_ty) => *nil_t,
        Cons(t1, t2) => cons_t.subst(head_var, *t1).subst(tail_var, *t2),
        _ => panic!("BUG: attempted to lcase with non list"),
    }
}

fn eval_cons(_: Box<Term>, _: Box<Term>) -> Term {
    panic!("BUG: attempted to step cons")
}

fn eval_inl(_: Box<Term>) -> Term {
    panic!("BUG: attempted to step inl of value")
}

fn eval_inr(_: Box<Term>) -> Term {
    panic!("BUG: attempted to step inr of value")
}

fn eval_case(
    t: Box<Term>,
    inl_var: &str,
    inl_t: Box<Term>,
    inr_var: &str,
    inr_t: Box<Term>,
) -> Term {
    match *t {
        Inl(t1, _ty) => inl_t.subst(inl_var, *t1),
        Inr(t2, _ty) => inr_t.subst(inr_var, *t2),
        _ => panic!("BUG: attempted to case with non inl or inr"),
    }
}

fn eval_fix(t: Box<Term>) -> Term {
    let (var, body) = if let Abs { var, ty: _, body } = t.as_ref() {
        (var.clone(), body.clone())
    } else {
        panic!("BUG: Fix on non Abs: {t}");
    };
    body.subst(&var, Fix(t))
}

fn eval_print(_t: Box<Term>) -> Term {
    panic!("BUG: trying to step print")
}

fn eval_pure(_t: Box<Term>) -> Term {
    panic!("BUG: trying to step pure")
}

fn eval_bind(_func: Box<Term>, _t: Box<Term>) -> Term {
    panic!("BUG: trying to step pure")
}

fn eval_tapp(t: Box<Term>, ty: NamedType) -> Term {
    if let TAbs { var: _, body } = *t {
        body.subst_ty(0, ty.shift(1, 0))
            .unshift_ty(1, 0)
            .expect("BUG: invalid unshift in eval_tapp")
    } else {
        panic!("BUG: attempted to apply non polymorphic abstraction to a type");
    }
}

impl Term {
    pub fn step(self, env: &HashMap<String, Term>) -> Self {
        match self {
            Var(y) => env
                .get(&y)
                .expect("BUG: cannot evaluate a variable: {y}")
                .clone(),
            App(t1, t2) => step_op2(App, eval_app, t1, t2, env),

            Let { var, val_t, body } => step_op1(
                |val_t| Let {
                    var: var.clone(),
                    val_t,
                    body: body.clone(),
                },
                |val_t| eval_let(val_t, &var, body.clone()),
                val_t,
                env,
            ),

            Compose(t1, t2) => step_op2(Compose, eval_comp, t1, t2, env),

            Ite {
                cond,
                if_true,
                if_false,
            } => step_op1(
                |cond| Ite {
                    cond,
                    if_true: if_true.clone(),
                    if_false: if_false.clone(),
                },
                |cond| eval_ite(cond, if_true.clone(), if_false.clone()),
                cond,
                env,
            ),

            Add(t1, t2) => step_op2(Add, eval_add, t1, t2, env),
            Sub(t1, t2) => step_op2(Sub, eval_sub, t1, t2, env),
            Mul(t1, t2) => step_op2(Mul, eval_mul, t1, t2, env),
            Div(t1, t2) => step_op2(Div, eval_div, t1, t2, env),

            Eq(t1, t2) => step_op2(Eq, eval_cmp!(==), t1, t2, env),
            Ne(t1, t2) => step_op2(Ne, eval_cmp!(!=), t1, t2, env),
            Lt(t1, t2) => step_op2(Lt, eval_cmp!(<), t1, t2, env),
            Le(t1, t2) => step_op2(Le, eval_cmp!(<=), t1, t2, env),
            Gt(t1, t2) => step_op2(Gt, eval_cmp!(>), t1, t2, env),
            Ge(t1, t2) => step_op2(Ge, eval_cmp!(>=), t1, t2, env),

            Pair(t1, t2) => step_op2(Pair, eval_pair, t1, t2, env),

            Fst(t) => step_op1(Fst, eval_fst, t, env),
            Snd(t) => step_op1(Snd, eval_snd, t, env),

            Cons(t1, t2) => step_op2(Cons, eval_cons, t1, t2, env),
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => step_op1(
                |t| LCase {
                    t,
                    nil_t: nil_t.clone(),
                    head_var: head_var.clone(),
                    tail_var: tail_var.clone(),
                    cons_t: cons_t.clone(),
                },
                |t| eval_lcase(t, nil_t.clone(), &head_var, &tail_var, cons_t.clone()),
                t,
                env,
            ),

            Inl(t, ty) => step_op1(move |t| Inl(t, ty), eval_inl, t, env),
            Inr(t, ty) => step_op1(move |t| Inr(t, ty), eval_inr, t, env),
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => step_op1(
                |t| Case {
                    t,
                    inl_var: inl_var.clone(),
                    inl_t: inl_t.clone(),
                    inr_var: inr_var.clone(),
                    inr_t: inr_t.clone(),
                },
                |t| eval_case(t, &inl_var, inl_t.clone(), &inr_var, inr_t.clone()),
                t,
                env,
            ),

            Fix(t) => step_op1(Fix, eval_fix, t, env),

            TApp(term, ty) => step_op1(
                |t| TApp(t, ty.clone()),
                |t| eval_tapp(t, ty.clone()),
                term,
                env,
            ),

            Panic(false, ty, term) => step_op1(
                |t| Panic(false, ty.clone(), t),
                |t| {
                    stlcpp_stderr!("*** STLC++ PANIC: {}", t);
                    Panic(true, ty.clone(), t)
                },
                term,
                env,
            ),

            Trace(i, term) => {
                if i == 0 || term.is_value() {
                    stlcpp_stderr!("*** STLC++ TRACE: {term}");
                    *term
                } else {
                    Trace(i - 1, Box::new(term.step(env)))
                }
            }

            Print(t) => step_op1(Print, eval_print, t, env),
            IOPure(ty, term) => step_op1(|t| IOPure(ty.clone(), t), eval_pure, term, env),
            IOBind { dom, cod, func, t } => step_op2(
                |func, t| IOBind {
                    dom: dom.clone(),
                    cod: cod.clone(),
                    func,
                    t,
                },
                eval_bind,
                func,
                t,
                env,
            ),

            _ => panic!("BUG: cannot step a value {:#?}", self),
        }
    }

    pub fn multistep(mut self, env: &HashMap<String, Term>) -> Self {
        while !self.is_value() && !self.is_err() {
            self = self.step(env)
        }
        self
    }
}
