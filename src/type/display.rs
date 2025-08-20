use crate::r#type::named_type::NamedType;

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(i) = self.subscript {
            write!(f, "{}_{i}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

/// The ctx must have increasing order of subscripts
pub fn fresh_name(ctx: &[Name], name: String) -> Name {
    if let Some(dup) = ctx.iter().rev().find(|n| n.name == name) {
        let next_sub = dup.subscript.unwrap_or_default() + 1;
        Name {
            name,
            subscript: Some(next_sub),
        }
    } else {
        // No conflict
        Name {
            name,
            subscript: None,
        }
    }
}

/// A name represents a type variable name that is possibly shadowed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name {
    name: String,
    subscript: Option<usize>,
}

use NamedType::*;

impl NamedType {
    pub fn fmt_ctx(&self, f: &mut std::fmt::Formatter<'_>, mut ctx: Vec<Name>) -> std::fmt::Result {
        match self {
            Boolean => write!(f, "Bool"),
            Integer => write!(f, "Int"),
            Character => write!(f, "Char"),
            Unit => write!(f, "Unit"),
            Var(i) => {
                assert!(ctx.len() - *i > 0);
                let name = ctx
                    .get(ctx.len() - *i - 1)
                    .expect("ctx should contain the index");
                write!(f, "{name}")
            }
            Abs(name, t) => {
                let fresh = fresh_name(&ctx, name.clone());
                write!(f, "forall {fresh}, ")?;
                ctx.push(fresh);
                t.fmt_ctx(f, ctx)
            }
            Arrow(ty1, ty2) => match (&**ty1, &**ty2) {
                (ty1, ty2) if ty1.needs_parens_arrow() => {
                    write!(f, "(")?;
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") -> ")?;
                    ty2.fmt_ctx(f, ctx)
                }
                _ => {
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " -> ")?;
                    ty2.fmt_ctx(f, ctx)
                }
            },
            Prod(ty1, ty2) => match (&**ty1, &**ty2) {
                (ty1, ty2) if ty1.needs_parens_prod() && ty2.needs_parens_prod() => {
                    write!(f, "(")?;
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") × (")?;
                    ty2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
                (ty1, ty2) if ty1.needs_parens_prod() => {
                    write!(f, "(")?;
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") × ")?;
                    ty2.fmt_ctx(f, ctx)
                }
                (ty1, ty2) if ty2.needs_parens_prod() || matches!(ty2, Prod { .. }) => {
                    // Prod associates left
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " × (")?;
                    ty2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
                // Otherwise no parens
                _ => {
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " × ")?;
                    ty2.fmt_ctx(f, ctx)
                }
            },
            List(ty) => {
                write!(f, "[")?;
                ty.fmt_ctx(f, ctx)?;
                write!(f, "]")
            }
            Sum(ty1, ty2) => match (&**ty1, &**ty2) {
                (ty1, ty2) if ty1.needs_parens_arrow() && ty2.needs_parens_arrow() => {
                    write!(f, "(")?;
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") + (")?;
                    ty2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
                (ty1, ty2) if ty1.needs_parens_arrow() => {
                    write!(f, "(")?;
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, ") + ")?;
                    ty2.fmt_ctx(f, ctx)
                }
                (ty1, ty2) if ty2.needs_parens_arrow() || matches!(ty2, Sum { .. }) => {
                    // Sum associates left
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " + (")?;
                    ty2.fmt_ctx(f, ctx)?;
                    write!(f, ")")
                }
                // Otherwise no parens
                _ => {
                    ty1.fmt_ctx(f, ctx.clone())?;
                    write!(f, " + ")?;
                    ty2.fmt_ctx(f, ctx)
                }
            },
            Hole => write!(f, "_"),
            IO(ty) => {
                write!(f, "IO (")?;
                ty.fmt_ctx(f, ctx)?;
                write!(f, ")")
            }
        }
    }
}
