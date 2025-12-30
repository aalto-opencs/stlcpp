use std::collections::HashMap;

use crate::r#type::named_type::NamedType;

use super::Term::{self, *};

impl Term {
    /// Performs substitution of a variable `x` with a given term `v`.
    ///
    /// # Incomplete Terms
    ///
    /// If `v` contains free variables, the function's behavior remains well-defined
    /// but the correctness of the result is not guaranteed (i.e. this function assumes that `v`
    /// is complete), though it must not panic even if it is not.
    ///
    /// # Examples
    ///
    /// **Variable substitution:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert_eq!(var("x").subst("x", var("y")), var("y"));
    /// ```
    ///
    /// **Inside an abstraction (bound variable remains unchanged):**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// # use stlcpp::r#type::named_type::NamedType::Boolean;
    /// assert_eq!(abs("x", Boolean, var("x")).subst("x", var("y")), abs("x", Boolean, var("x")));
    /// ```
    ///
    /// **Using a let expression:**
    /// In the let expression below, the bound variable is `"y"`. Substituting `"x"` will
    /// affect both the value part and the body.
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// let let_expr = letin("y", var("x"), app(var("x"), var("y")));
    /// let expected  = letin("y", var("z"), app(var("z"), var("y")));
    /// assert_eq!(let_expr.subst("x", var("z")), expected);
    /// ```
    ///
    /// Substituting inside the body of a let should only substitute if the var is different.
    /// - `[x ↦ id](let x = id in x) = let x = id in x`, i.e. should be invariant, as `x` is bound by the let.
    /// - `[x ↦ id](let x = x in x) = let x = id in x`, i.e. `val_t` is substituted, as it's not quantified by the let.
    ///
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert_eq!(
    ///     letin("x", id2(), var("x")).subst("x", id2()),
    ///     letin("x", id2(), var("x"))
    /// );
    /// assert_eq!(
    ///     letin("x", var("x"), var("x")).subst("x", id2()),
    ///     letin("x", id2(), var("x"))
    /// );
    /// ```
    ///
    /// **LCase expression:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// let lcase_expr = lcase("x", var("x"), "h", "t", var("x"));
    /// let expected  = lcase("z", var("z"), "h", "t", var("z"));
    /// assert_eq!(lcase_expr.subst("x", var("z")), expected);
    /// ```
    ///
    /// As the `nil` case doesn't bind any variables, it's always substituted recursively, but the `cons` case binds two variables, which substitution must respect.
    /// - `[x ↦ id](lcase x of | nil => x | cons x xs => x) = lcase id of | nil => id | cons x xs => x`, because the `x` is bound by the `cons` branch.
    /// - `[xs ↦ id](lcase x of | nil => x | cons x xs => xs) = lcase x of | nil => x | cons x xs => xs`, because the `xs` is bound by the `cons` branch.
    ///
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert_eq!(
    ///     lcase(var("x"), var("x"), "x", "xs", var("x")).subst("x", id2()),
    ///     lcase(id2(), id2(), "x", "xs", var("x"))
    /// );
    /// assert_eq!(
    ///     lcase(var("x"), var("x"), "x", "xs", var("xs")).subst("xs", id2()),
    ///     lcase(var("x"), var("x"), "x", "xs", var("xs"))
    /// );
    /// ```
    ///
    /// **Sum types:**
    ///
    /// Substituting inside `inl` and `inr` works recursively.
    ///
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// # use stlcpp::r#type::named_type::NamedType::Boolean;
    /// assert_eq!(inl(var("x"), Boolean).subst("x", var("z")), inl(var("z"), Boolean));
    /// assert_eq!(inr(var("x"), Boolean).subst("x", var("z")), inr(var("z"), Boolean));
    /// ```
    ///
    /// Substituting in a case expression:
    ///
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// # use stlcpp::r#type::named_type::NamedType::Boolean;
    /// # use num_bigint::BigInt;
    /// let t = case(
    ///     inl(var("x"), Boolean),
    ///     "x",
    ///     var("x"),
    ///     "y",
    ///     eq(var("x"), Int(BigInt::from(0))),
    /// );
    /// // Apply the substitution `[x ↦ Int(42)]`.
    /// // In the inl branch, the binder "x" shadows the substitution, so that branch remains unchanged.
    /// // In the inr branch, substitution is applied to `eq(var("x"), Int(0))`,
    /// // yielding `eq(Int(42), Int(0))`.
    /// let expected = case(
    ///     inl(Int(BigInt::from(42)), Boolean),
    ///     "x",
    ///     var("x"),
    ///     "y",
    ///     eq(Int(BigInt::from(42)), Int(BigInt::from(0))),
    /// );
    /// assert_eq!(t.subst("x", Int(BigInt::from(42))), expected);
    ///
    /// let t = case(
    ///     inr(var("y"), Boolean),
    ///     "x",
    ///     eq(var("y"), Int(BigInt::from(0))),
    ///     "y",
    ///     var("y"),
    /// );
    /// // Apply the substitution `[y ↦ True]`.
    /// // In the inl branch, substitution applies to `eq(var("y"), Int(0)) => eq(True, Int(0))`.
    /// // In the inr branch, the binder "y" shadows the substitution, so that branch remains unchanged.
    /// let expected = case(
    ///     inr(True, Boolean),
    ///     "x",
    ///     eq(True, Int(BigInt::from(0))),
    ///     "y",
    ///     var("y"),
    /// );
    /// assert_eq!(t.subst("y", True), expected);
    /// ```

    pub fn subst(mut self, x: &str, v: Self) -> Self {
        self.subst_in_place(x, v);
        self
    }

    pub fn subst_in_place(&mut self, x: &str, v: Self) {
        let mut map = HashMap::new();
        map.insert(x, v);
        self.subst_all(map)
    }

    pub fn subst_all<'a>(&mut self, mut map: HashMap<&'a str, Term>) {
        match self {
            Var(y) => {
                if let Some(t) = map.get(y.as_str()) {
                    *self = t.clone();
                }
            }
            Abs { var, body, .. } => {
                map.remove(var.as_str());
                body.subst_all(map);
            }
            App(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Let { var, val_t, body } => {
                val_t.subst_all(map.clone());
                map.remove(var.as_str());
                body.subst_all(map);
            }
            Compose(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Ite {
                cond,
                if_true,
                if_false,
            } => {
                cond.subst_all(map.clone());
                if_true.subst_all(map.clone());
                if_false.subst_all(map);
            }
            Add(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Sub(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Mul(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Div(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Eq(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Ne(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Lt(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Le(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Gt(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Ge(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Pair(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            Fst(t) => {
                t.subst_all(map);
            }
            Snd(t) => {
                t.subst_all(map);
            }
            Cons(t1, t2) => {
                t1.subst_all(map.clone());
                t2.subst_all(map);
            }
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => {
                t.subst_all(map.clone());
                nil_t.subst_all(map.clone());
                map.remove(head_var.as_str());
                map.remove(tail_var.as_str());
                cons_t.subst_all(map);
            }
            Inl(t1, _ty) => {
                t1.subst_all(map);
            }
            Inr(t1, _ty) => {
                t1.subst_all(map);
            }
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => {
                t.subst_all(map.clone());
                let mut inl_map = map.clone();
                inl_map.remove(inl_var.as_str());
                inl_t.subst_all(inl_map);
                map.remove(inr_var.as_str());
                inr_t.subst_all(map);
            }
            Fix(t1) => {
                t1.subst_all(map);
            }
            TAbs { var: _var, body } => {
                body.subst_all(map);
            }
            TApp(term, _ty) => {
                term.subst_all(map);
            }
            Trace(_i, term) => {
                term.subst_all(map);
            }
            Panic(_b, _ty, term) => {
                term.subst_all(map);
            }
            Print(t1) => {
                t1.subst_all(map);
            }
            IOPure(_ty, term) => {
                term.subst_all(map);
            }
            IOBind { func, t, .. } => {
                func.subst_all(map.clone());
                t.subst_all(map);
            }
            True | False | Int(_) | Char(_) | Nil(_) | Trivial | ReadLine => {}
        }
    }

    pub fn subst_ty(self, x: usize, v_ty: NamedType) -> Self {
        match self {
            Abs { var, ty, body } => Abs {
                var,
                ty: ty.subst(x, v_ty.clone()),
                body: Box::new(body.subst_ty(x, v_ty)),
            },
            App(t1, t2) => App(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Let { var, val_t, body } => Let {
                body: Box::new(body.subst_ty(x, v_ty.clone())),
                var,
                val_t: Box::new(val_t.subst_ty(x, v_ty)),
            },
            Compose(t1, t2) => Compose(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Ite {
                cond,
                if_true,
                if_false,
            } => Ite {
                cond: Box::new(cond.subst_ty(x, v_ty.clone())),
                if_true: Box::new(if_true.subst_ty(x, v_ty.clone())),
                if_false: Box::new(if_false.subst_ty(x, v_ty)),
            },
            Add(t1, t2) => Add(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Sub(t1, t2) => Sub(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Mul(t1, t2) => Mul(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Div(t1, t2) => Div(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Eq(t1, t2) => Eq(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Ne(t1, t2) => Ne(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Lt(t1, t2) => Lt(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Le(t1, t2) => Le(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Gt(t1, t2) => Gt(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Ge(t1, t2) => Ge(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            Pair(t1, t2) => Pair(
                t1.subst_ty(x, v_ty.clone()).into(),
                t2.subst_ty(x, v_ty).into(),
            ),
            Fst(t) => Fst(t.subst_ty(x, v_ty).into()),
            Snd(t) => Snd(t.subst_ty(x, v_ty).into()),
            Nil(ty) => Nil(ty.subst(x, v_ty)),
            Cons(t1, t2) => Cons(
                Box::new(t1.subst_ty(x, v_ty.clone())),
                Box::new(t2.subst_ty(x, v_ty)),
            ),
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => LCase {
                t: Box::new(t.subst_ty(x, v_ty.clone())),
                nil_t: Box::new(nil_t.subst_ty(x, v_ty.clone())),
                cons_t: Box::new(cons_t.subst_ty(x, v_ty)),
                head_var,
                tail_var,
            },
            Inl(t1, ty) => Inl(Box::new(t1.subst_ty(x, v_ty.clone())), ty.subst(x, v_ty)),
            Inr(t1, ty) => Inr(Box::new(t1.subst_ty(x, v_ty.clone())), ty.subst(x, v_ty)),
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => Case {
                t: Box::new(t.subst_ty(x, v_ty.clone())),
                inl_t: Box::new(inl_t.subst_ty(x, v_ty.clone())),
                inl_var,
                inr_t: Box::new(inr_t.subst_ty(x, v_ty)),
                inr_var,
            },
            Fix(t1) => Fix(Box::new(t1.subst_ty(x, v_ty.clone()))),
            TAbs { var, body } => TAbs {
                body: Box::new(body.subst_ty(x + 1, v_ty.shift(1, 0))), // Shift because we enter a deeper type variable context
                var,
            },
            TApp(term, ty) => TApp(Box::new(term.subst_ty(x, v_ty.clone())), ty.subst(x, v_ty)),
            Panic(b, ty, term) => Panic(
                b,
                ty.subst(x, v_ty.clone()),
                Box::new(term.subst_ty(x, v_ty)),
            ),
            Trace(i, term) => Trace(i, Box::new(term.subst_ty(x, v_ty.clone()))),
            Print(t1) => Print(Box::new(t1.subst_ty(x, v_ty.clone()))),
            IOPure(ty, term) => IOPure(ty.subst(x, v_ty.clone()), Box::new(term.subst_ty(x, v_ty))),
            IOBind { dom, cod, func, t } => IOBind {
                dom: dom.subst(x, v_ty.clone()),
                cod: cod.subst(x, v_ty.clone()),
                func: Box::new(func.subst_ty(x, v_ty.clone())),
                t: Box::new(t.subst_ty(x, v_ty.clone())),
            },
            Var(_) | True | False | Int(_) | Char(_) | Trivial | ReadLine => self,
        }
    }

    pub fn unshift_ty(self, amount: usize, cutoff: usize) -> Option<Self> {
        match self {
            Var(x) => Some(Var(x)),
            Abs { var, ty, body } => Some(Abs {
                var,
                ty: ty.unshift(amount, cutoff)?,
                body: Box::new(body.unshift_ty(amount, cutoff)?),
            }),
            App(t1, t2) => Some(App(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Let { var, val_t, body } => Some(Let {
                var,
                val_t: Box::new(val_t.unshift_ty(amount, cutoff)?),
                body: Box::new(body.unshift_ty(amount, cutoff)?),
            }),
            Compose(t1, t2) => Some(Compose(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            True => Some(True),
            False => Some(False),
            Ite {
                cond,
                if_true,
                if_false,
            } => Some(Ite {
                cond: Box::new(cond.unshift_ty(amount, cutoff)?),
                if_true: Box::new(if_true.unshift_ty(amount, cutoff)?),
                if_false: Box::new(if_false.unshift_ty(amount, cutoff)?),
            }),
            Int(n) => Some(Int(n)),
            Add(t1, t2) => Some(Add(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Sub(t1, t2) => Some(Sub(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Mul(t1, t2) => Some(Mul(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Div(t1, t2) => Some(Div(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Eq(t1, t2) => Some(Eq(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Ne(t1, t2) => Some(Ne(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Lt(t1, t2) => Some(Lt(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Le(t1, t2) => Some(Le(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Gt(t1, t2) => Some(Gt(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Ge(t1, t2) => Some(Ge(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Char(c) => Some(Char(c)),
            Trivial => Some(Trivial),
            Pair(t1, t2) => Some(Pair(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            Fst(t) => Some(Fst(Box::new(t.unshift_ty(amount, cutoff)?))),
            Snd(t) => Some(Snd(Box::new(t.unshift_ty(amount, cutoff)?))),
            Nil(ty) => Some(Nil(ty.unshift(amount, cutoff)?)),
            Cons(t1, t2) => Some(Cons(
                Box::new(t1.unshift_ty(amount, cutoff)?),
                Box::new(t2.unshift_ty(amount, cutoff)?),
            )),
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => Some(LCase {
                t: Box::new(t.unshift_ty(amount, cutoff)?),
                nil_t: Box::new(nil_t.unshift_ty(amount, cutoff)?),
                head_var,
                tail_var,
                cons_t: Box::new(cons_t.unshift_ty(amount, cutoff)?),
            }),
            Inl(t, ty) => Some(Inl(
                Box::new(t.unshift_ty(amount, cutoff)?),
                ty.unshift(amount, cutoff)?,
            )),
            Inr(t, ty) => Some(Inr(
                Box::new(t.unshift_ty(amount, cutoff)?),
                ty.unshift(amount, cutoff)?,
            )),
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => Some(Case {
                t: Box::new(t.unshift_ty(amount, cutoff)?),
                inl_var,
                inl_t: Box::new(inl_t.unshift_ty(amount, cutoff)?),
                inr_var,
                inr_t: Box::new(inr_t.unshift_ty(amount, cutoff)?),
            }),
            Fix(t) => Some(Fix(Box::new(t.unshift_ty(amount, cutoff)?))),
            TAbs { var, body } => Some(TAbs {
                var,
                body: Box::new(body.unshift_ty(amount, cutoff + 1)?),
            }),
            TApp(t, ty) => Some(TApp(
                Box::new(t.unshift_ty(amount, cutoff)?),
                ty.unshift(amount, cutoff)?,
            )),
            Panic(b, ty, t) => Some(Panic(
                b,
                ty.unshift(amount, cutoff)?,
                Box::new(t.unshift_ty(amount, cutoff)?),
            )),
            Trace(i, t) => Some(Trace(i, Box::new(t.unshift_ty(amount, cutoff)?))),
            Print(t) => Some(Print(Box::new(t.unshift_ty(amount, cutoff)?))),
            ReadLine => Some(ReadLine),
            IOPure(ty, term) => Some(IOPure(
                ty.unshift(amount, cutoff)?,
                Box::new(term.unshift_ty(amount, cutoff)?),
            )),
            IOBind { dom, cod, func, t } => Some(IOBind {
                dom: dom.unshift(amount, cutoff)?,
                cod: cod.unshift(amount, cutoff)?,
                func: Box::new(func.unshift_ty(amount, cutoff)?),
                t: Box::new(t.unshift_ty(amount, cutoff)?),
            }),
        }
    }

    // FIXME Iterator instance search goes into loop
    // pub fn subst(self, x: &str, v: Self) -> Self {
    //     self.subst_all(IndexedPair(x.to_string(), v).iter())
    // }

    // // TODO what is the difference between for<'a> Iterator in this case?
    // pub fn subst_all<'a, I: Iterator<Item = (&'a str, &'a Term)> + Clone>(
    //     self,
    //     mut map: I,
    // ) -> Self {
    //     match self {
    //         Var(ref y) => match map.find(|(k, _v)| k == y) {
    //             Some((_k, v)) => v.clone(),
    //             None => self,
    //         },
    //         Abs { var, ty, body } => Abs {
    //             var: var.clone(),
    //             body: Box::new(body.subst_all(map.filter(move |(k, _v)| k != &var))),
    //             ty,
    //         },
    //         _ => self,
    //     }
    // }
}
