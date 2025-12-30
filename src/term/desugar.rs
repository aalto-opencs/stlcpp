use std::collections::HashMap;
use std::marker::PhantomData;

use crate::{
    errors::Error,
    parse::Span,
    syntax::Syntaxes,
    term::tokens::{Desugared, SpannedToken, Surface, Token},
};

impl<'a> SpannedToken<'a, Surface> {
    /// Fully desugar a parsed term into a core-only token tree.
    ///
    /// If an infix operator is encountered with no corresponding syntax definition, this
    /// returns an error string.
    pub fn desugar(self, syntaxes: &'_ Syntaxes) -> Result<SpannedToken<'a, Desugared>, Error> {
        let position = self.position;
        use Token::*;

        let token = match self.token {
            Prefix { op, rhs } => {
                let rhs = rhs.desugar(syntaxes)?;

                let rule = syntaxes.find_prefix_rule(op).ok_or_else(|| {
                    Error::DesugarError(format!(
                        "unknown prefix operator '{op}' (no matching syntax rule)"
                    ))
                })?;

                let env: HashMap<&str, SpannedToken<'a, Desugared>> =
                    HashMap::from([(rule.rhs.as_str(), rhs)]);

                let body_desugared = rule.body.clone().desugar(syntaxes)?;
                subst_template_at_pos(&body_desugared.token, &env, position)?.token
            }
            Infix { op, lhs, rhs } => {
                // First desugar both sides (so nested usages are expanded).
                let lhs = lhs.desugar(syntaxes)?;
                let rhs = rhs.desugar(syntaxes)?;

                // Find matching rule by operator symbol.
                let rule = syntaxes.find_infix_rule(op).ok_or_else(|| {
                    Error::DesugarError(format!(
                        "unknown infix operator '{op}' (no matching syntax rule)"
                    ))
                })?;

                // Expand by substituting pattern vars in the rule body template (stored as an owned token tree).
                // Nodes introduced by the template get `position` so errors point to the call site.
                let env: HashMap<&str, SpannedToken<'a, Desugared>> =
                    HashMap::from([(rule.lhs.as_str(), lhs), (rule.rhs.as_str(), rhs)]);

                // We also desugar the body of the rule to ensure it doesn't also contain custom syntax
                let body_desugared = rule.body.clone().desugar(syntaxes)?;
                subst_template_at_pos(&body_desugared.token, &env, position)?.token
            }
            Var(v) => Var(v),
            Abs { var, ty, body } => Abs {
                var,
                ty,
                body: Box::new(body.desugar(syntaxes)?),
            },
            App(t1, t2) => App(
                Box::new(t1.desugar(syntaxes)?),
                Box::new(t2.desugar(syntaxes)?),
            ),
            Let { var, val_t, body } => Let {
                var,
                val_t: Box::new(val_t.desugar(syntaxes)?),
                body: Box::new(body.desugar(syntaxes)?),
            },
            Ite {
                cond,
                if_true,
                if_false,
            } => Ite {
                cond: Box::new(cond.desugar(syntaxes)?),
                if_true: Box::new(if_true.desugar(syntaxes)?),
                if_false: Box::new(if_false.desugar(syntaxes)?),
            },
            Add(a, b) => Add(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Sub(a, b) => Sub(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Mul(a, b) => Mul(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Div(a, b) => Div(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Eq(a, b) => Eq(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Ne(a, b) => Ne(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Lt(a, b) => Lt(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Le(a, b) => Le(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Gt(a, b) => Gt(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Ge(a, b) => Ge(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Pair(a, b) => Pair(
                Box::new(a.desugar(syntaxes)?),
                Box::new(b.desugar(syntaxes)?),
            ),
            Fst(t) => Fst(Box::new(t.desugar(syntaxes)?)),
            Snd(t) => Snd(Box::new(t.desugar(syntaxes)?)),
            Cons(h, tl) => Cons(
                Box::new(h.desugar(syntaxes)?),
                Box::new(tl.desugar(syntaxes)?),
            ),
            LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => LCase {
                t: Box::new(t.desugar(syntaxes)?),
                nil_t: Box::new(nil_t.desugar(syntaxes)?),
                head_var,
                tail_var,
                cons_t: Box::new(cons_t.desugar(syntaxes)?),
            },
            Inl(t, ty) => Inl(Box::new(t.desugar(syntaxes)?), ty),
            Inr(t, ty) => Inr(Box::new(t.desugar(syntaxes)?), ty),
            Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => Case {
                t: Box::new(t.desugar(syntaxes)?),
                inl_var,
                inl_t: Box::new(inl_t.desugar(syntaxes)?),
                inr_var,
                inr_t: Box::new(inr_t.desugar(syntaxes)?),
            },
            Fix(t) => Fix(Box::new(t.desugar(syntaxes)?)),
            TAbs { var, body } => TAbs {
                var,
                body: Box::new(body.desugar(syntaxes)?),
            },
            TApp(t, ty) => TApp(Box::new(t.desugar(syntaxes)?), ty),
            Panic(b, ty, t) => Panic(b, ty, Box::new(t.desugar(syntaxes)?)),
            Trace(n, t) => Trace(n, Box::new(t.desugar(syntaxes)?)),
            Print(t) => Print(Box::new(t.desugar(syntaxes)?)),
            IOPure(ty, t) => IOPure(ty, Box::new(t.desugar(syntaxes)?)),
            IOBind { dom, cod, func, t } => IOBind {
                dom: dom,
                cod: cod,
                func: Box::new(func.desugar(syntaxes)?),
                t: Box::new(t.desugar(syntaxes)?),
            },
            True => True,
            False => False,
            Int(n) => Int(n),
            Char(c) => Char(c),
            Trivial => Trivial,
            Nil(ty) => Nil(ty),
            ReadLine => ReadLine,
        };

        Ok(SpannedToken {
            position,
            token,
            _state: PhantomData,
        })
    }
}

fn subst_template_at_pos<'a>(
    template: &'_ Token<'a, Desugared>,
    env: &'_ HashMap<&str, SpannedToken<'a, Desugared>>,
    position: Span<'a>,
) -> Result<SpannedToken<'a, Desugared>, Error> {
    if let Token::Var(v) = template {
        if let Some(replacement) = env.get(v.as_str()) {
            return Ok(replacement.clone());
        }
    }

    // Helper: recurse on children without borrowing locals.
    let rec = |t: &'_ Token<'a, Desugared>,
               env: &'_ HashMap<&str, SpannedToken<'a, Desugared>>|
     -> Result<SpannedToken<'a, Desugared>, Error> {
        subst_template_at_pos(t, env, position)
    };

    use Token::*;
    let token: Token<'a, Desugared> = match template {
        Var(v) => Var(v.clone()),
        True => True,
        False => False,
        Int(n) => Int(n.clone()),
        Char(c) => Char(*c),
        Trivial => Trivial,
        Nil(ty) => Nil(ty.clone()),
        ReadLine => ReadLine,
        Abs { var, ty, body } => Abs {
            var: var.clone(),
            ty: ty.clone(),
            body: Box::new(rec(&body.token, env)?),
        },
        App(t1, t2) => App(
            Box::new(rec(&t1.token, env)?),
            Box::new(rec(&t2.token, env)?),
        ),
        Let { var, val_t, body } => Let {
            var: var.clone(),
            val_t: Box::new(rec(&val_t.token, env)?),
            body: Box::new(rec(&body.token, env)?),
        },
        Ite {
            cond,
            if_true,
            if_false,
        } => Ite {
            cond: Box::new(rec(&cond.token, env)?),
            if_true: Box::new(rec(&if_true.token, env)?),
            if_false: Box::new(rec(&if_false.token, env)?),
        },
        Add(a, b) => Add(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Sub(a, b) => Sub(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Mul(a, b) => Mul(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Div(a, b) => Div(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Eq(a, b) => Eq(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Ne(a, b) => Ne(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Lt(a, b) => Lt(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Le(a, b) => Le(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Gt(a, b) => Gt(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Ge(a, b) => Ge(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Pair(a, b) => Pair(Box::new(rec(&a.token, env)?), Box::new(rec(&b.token, env)?)),
        Fst(t) => Fst(Box::new(rec(&t.token, env)?)),
        Snd(t) => Snd(Box::new(rec(&t.token, env)?)),
        Cons(h, tl) => Cons(
            Box::new(rec(&h.token, env)?),
            Box::new(rec(&tl.token, env)?),
        ),
        LCase {
            t,
            nil_t,
            head_var,
            tail_var,
            cons_t,
        } => LCase {
            t: Box::new(rec(&t.token, env)?),
            nil_t: Box::new(rec(&nil_t.token, env)?),
            head_var: head_var.clone(),
            tail_var: tail_var.clone(),
            cons_t: Box::new(rec(&cons_t.token, env)?),
        },
        Inl(t, ty) => Inl(Box::new(rec(&t.token, env)?), ty.clone()),
        Inr(t, ty) => Inr(Box::new(rec(&t.token, env)?), ty.clone()),
        Case {
            t,
            inl_var,
            inl_t,
            inr_var,
            inr_t,
        } => Case {
            t: Box::new(rec(&t.token, env)?),
            inl_var: inl_var.clone(),
            inl_t: Box::new(rec(&inl_t.token, env)?),
            inr_var: inr_var.clone(),
            inr_t: Box::new(rec(&inr_t.token, env)?),
        },
        Fix(t) => Fix(Box::new(rec(&t.token, env)?)),
        TAbs { var, body } => TAbs {
            var: var.clone(),
            body: Box::new(rec(&body.token, env)?),
        },
        TApp(t, ty) => TApp(Box::new(rec(&t.token, env)?), ty.clone()),
        Panic(b, ty, t) => Panic(*b, ty.clone(), Box::new(rec(&t.token, env)?)),
        Trace(n, t) => Trace(*n, Box::new(rec(&t.token, env)?)),
        Print(t) => Print(Box::new(rec(&t.token, env)?)),
        IOPure(ty, t) => IOPure(ty.clone(), Box::new(rec(&t.token, env)?)),
        IOBind { dom, cod, func, t } => IOBind {
            dom: dom.clone(),
            cod: cod.clone(),
            func: Box::new(rec(&func.token, env)?),
            t: Box::new(rec(&t.token, env)?),
        },
        Infix { .. } | Prefix { .. } => {
            unreachable!("BUG: desugared should not contain custom syntax")
        }
    };

    Ok(SpannedToken {
        position,
        token,
        _state: PhantomData,
    })
}
