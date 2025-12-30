use crate::r#type::{
    display::{Name, fresh_name},
    named_type::NamedType,
};

use super::Term::{self, *};

/// Formats a [`Term::Cons`] recursively as a list literal.
///
/// - Proper lists end in [`Term::Nil`] and are formatted as `[1, 2, 3]`.
/// - If the tail is not `Nil` (e.g. a variable), render it as `[1, 2, ...x]`.
/// - If all elements are [`Term::Char`] and the tail is `Nil`, render as a string literal.
pub(crate) fn fmt_list_ctx(
    f: &mut std::fmt::Formatter<'_>,
    ty_ctx: Vec<Name>,
    t: &Term,
) -> std::fmt::Result {
    fn recurse<'a>(terms: &mut Vec<&'a Term>, t: &'a Term) {
        if let Cons(head, tail) = t {
            terms.push(head);
            recurse(terms, tail);
        } else {
            terms.push(t);
        }
    }

    let mut terms = Vec::<&Term>::new();
    recurse(&mut terms, t);

    let last = terms.pop().expect("BUG: terms is non-empty");

    // If the list is a string literal, format it as one
    if matches!(last, Nil(_)) && terms.iter().all(|t| matches!(t, Char(_))) {
        write!(f, "\"")?;
        for term in terms {
            let Char(c) = term else {
                panic!("BUG: impossible")
            };
            // Reuse Rust's debug formatting for chars to keep escapes correct,
            // but strip the surrounding quotes.
            let s = format!("{c:?}");
            let inner = s
                .strip_prefix('\'')
                .and_then(|s| s.strip_suffix('\''))
                .unwrap_or(&s);
            write!(f, "{inner}")?;
        }
        write!(f, "\"")
    } else {
        write!(f, "[")?;
        for (i, term) in terms.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            term.fmt_ctx(f, ty_ctx.clone())?;
        }

        // the list might end in a var, in which case we render it as [1, 2, ...x]
        match last {
            Nil(_) => write!(f, "]"),
            t => {
                if !terms.is_empty() {
                    write!(f, ", ")?;
                }
                write!(f, "...")?;
                t.fmt_ctx(f, ty_ctx)?;
                write!(f, "]")
            }
        }
    }
}

pub(crate) fn to_string_raw(t: &Term) -> String {
    fn recurse<'a>(terms: &mut Vec<&'a Term>, t: &'a Term) {
        if let Cons(head, tail) = t {
            terms.push(head);
            recurse(terms, tail);
        } else {
            terms.push(t);
        }
    }

    let mut terms = Vec::<&Term>::new();
    recurse(&mut terms, t);
    let _ = terms.pop();

    let mut s = String::new();
    for term in terms {
        let Char(c) = term else {
            panic!("BUG: not a string")
        };
        s.push(*c);
    }
    s
}

impl Term {
    /// Pretty-print a term with a *type-variable* context.
    ///
    /// `ctx` is a stack of bound type variable names, outermost-to-innermost.
    /// This is used to print De Bruijn type variables inside [`NamedType`]
    /// via [`NamedType::fmt_ctx`].
    pub fn fmt_ctx(&self, f: &mut std::fmt::Formatter<'_>, mut ctx: Vec<Name>) -> std::fmt::Result {
        match self {
            Var(x) => write!(f, "{x}"),
            Abs { var, ty, body } => {
                match ty {
                    // Parenthesize polymorphic types to avoid ambiguity: fun x : (forall ...), ...
                    NamedType::Abs(..) => {
                        write!(f, "fun {var} : (")?;
                        ty.fmt_ctx(f, ctx.clone())?;
                        write!(f, "), ")?;
                    }
                    _ => {
                        write!(f, "fun {var} : ")?;
                        ty.fmt_ctx(f, ctx.clone())?;
                        write!(f, ", ")?;
                    }
                }

                body.fmt_ctx(f, ctx)
            }
            App(term1, term2) => match (&**term1, &**term2) {
                (term1 @ (Var(_) | True | False), term2 @ (Var(_) | True | False)) => {
                    term1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " ")?;
                    term2.fmt_ctx(f, ctx)
                }
                (term1 @ Var(_), term2) => {
                    term1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " (")?;
                    term2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
                (term1, term2 @ (Var(_) | True | False)) => {
                    write!(f, "(")?;
                    term1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") ")?;
                    term2.fmt_ctx(f, ctx)
                }
                _ => {
                    write!(f, "(")?;
                    term1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") (")?;
                    term2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
            },
            Let { var, val_t, body } => {
                write!(f, "let {var} = ")?;
                val_t.fmt_ctx(f, ctx.clone())?;
                write!(f, " in ")?;
                body.fmt_ctx(f, ctx)
            }
            Compose(t1, t2) => {
                write!(f, "(")?;
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " . ")?;
                t2.fmt_ctx(f, ctx)?;
                write!(f, ")")
            }
            True => write!(f, "true"),
            False => write!(f, "false"),
            Ite {
                cond,
                if_true,
                if_false,
            } => {
                write!(f, "if ")?;
                cond.fmt_ctx(f, ctx.clone())?;
                write!(f, " then ")?;
                if_true.fmt_ctx(f, ctx.clone())?;
                write!(f, " else ")?;
                if_false.fmt_ctx(f, ctx)
            }
            Int(n) => write!(f, "{n}"),
            Add(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " + ")?;
                t2.fmt_ctx(f, ctx)
            }
            Sub(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " - ")?;
                t2.fmt_ctx(f, ctx)
            }
            Mul(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " * ")?;
                t2.fmt_ctx(f, ctx)
            }
            Div(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " / ")?;
                t2.fmt_ctx(f, ctx)
            }
            Eq(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " == ")?;
                t2.fmt_ctx(f, ctx)
            }
            Ne(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " != ")?;
                t2.fmt_ctx(f, ctx)
            }
            Lt(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " < ")?;
                t2.fmt_ctx(f, ctx)
            }
            Le(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " <= ")?;
                t2.fmt_ctx(f, ctx)
            }
            Gt(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " > ")?;
                t2.fmt_ctx(f, ctx)
            }
            Ge(t1, t2) => {
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, " >= ")?;
                t2.fmt_ctx(f, ctx)
            }
            Char(c) => write!(f, "{c:?}"),
            Trivial => write!(f, "()"),
            Pair(t1, t2) => {
                write!(f, "(")?;
                t1.fmt_ctx(f, ctx.clone())?;
                write!(f, ", ")?;
                t2.fmt_ctx(f, ctx)?;
                write!(f, ")")
            }
            Fst(t) => {
                write!(f, "fst ")?;
                t.fmt_ctx(f, ctx)
            }
            Snd(t) => {
                write!(f, "snd ")?;
                t.fmt_ctx(f, ctx)
            }
            Nil(_) => write!(f, "[]"),
            list @ Cons(_, _) => fmt_list_ctx(f, ctx.clone(), list),
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => {
                write!(f, "lcase ")?;
                t.fmt_ctx(f, ctx.clone())?;
                write!(f, " of | nil => ")?;
                nil_t.fmt_ctx(f, ctx.clone())?;
                write!(f, " | cons {head_var} {tail_var} => ")?;
                cons_t.fmt_ctx(f, ctx)
            }
            Inl(term, _right_ty) => {
                write!(f, "inl ")?;
                term.fmt_ctx(f, ctx)
            }
            Inr(term, _left_ty) => {
                write!(f, "inr ")?;
                term.fmt_ctx(f, ctx)
            }
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => {
                write!(f, "case ")?;
                t.fmt_ctx(f, ctx.clone())?;
                write!(f, " of | inl {inl_var} => ")?;
                inl_t.fmt_ctx(f, ctx.clone())?;
                write!(f, " | inr {inr_var} => ")?;
                inr_t.fmt_ctx(f, ctx)
            }
            Fix(t) => {
                write!(f, "fix ")?;
                t.fmt_ctx(f, ctx)
            }
            TAbs { var, body } => {
                let fresh = fresh_name(&ctx, var.clone());
                write!(f, "fun {fresh}, ")?;
                ctx.push(fresh);
                body.fmt_ctx(f, ctx)
            }
            TApp(term, ty) => {
                term.fmt_ctx(f, ctx.clone())?;
                write!(f, " ")?;
                ty.fmt_ctx(f, ctx)
            }
            Panic(_, ty, t) => {
                write!(f, "panic ")?;
                ty.fmt_ctx(f, ctx.clone())?;
                write!(f, " ")?;
                t.fmt_ctx(f, ctx)
            }
            Trace(i, t) => {
                write!(f, "trace {i} ")?;
                t.fmt_ctx(f, ctx)
            }
            Print(term) => {
                write!(f, "print ")?;
                term.fmt_ctx(f, ctx)
            }
            ReadLine => {
                write!(f, "readline")
            }
            IOPure(ty, term) => {
                write!(f, "pure ")?;
                ty.fmt_ctx(f, ctx.clone())?;
                write!(f, " ")?;
                term.fmt_ctx(f, ctx)
            }
            IOBind { dom, cod, func, t } => {
                // TODO add parentheses as needed
                write!(f, "bind ")?;
                dom.fmt_ctx(f, ctx.clone())?;
                write!(f, " ")?;
                cod.fmt_ctx(f, ctx.clone())?;
                write!(f, " (")?;
                func.fmt_ctx(f, ctx.clone())?;
                write!(f, ") ")?;
                t.fmt_ctx(f, ctx)
            }
        }
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_ctx(f, Vec::<Name>::new())
    }
}
