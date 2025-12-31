pub mod check;
pub mod display;
pub mod named_type;
pub mod parse;
pub mod subst;
pub mod tokens;
pub mod util;

use crate::{
    parse::Span,
    r#type::{display::Name, named_type::NamedType, tokens::SpannedToken},
};

#[derive(Debug, Clone)]
pub enum TypeError<'a> {
    UndefinedVariable(String),
    DuplicateDeclaration(String),
    TAppRightNotClosed(SpannedToken<'a>),
    NotProperlyFormed(String, (Span<'a>, NamedType), Vec<Name>),
    FreeTypeVariable(String),
    Shadowing(String),
    /// (Expected, got)
    Mismatch((Span<'a>, NamedType), (Span<'a>, NamedType), Vec<Name>),
    /// Can't compare ty1 with ty2
    CantCompare((Span<'a>, NamedType), (Span<'a>, NamedType), Vec<Name>),
    // TODO Holes(SpannedToken<'a>),
    UnknownSyntax(Span<'a>, String),
    Holes,
}

use TypeError::*;

pub(crate) fn draw_located_span(span: &Span<'_>, message: String) -> String {
    let beginning_of_line = span.extra[..span.location_offset()]
        .rfind('\n')
        .unwrap_or(0);
    let end_of_line = span.extra[span.location_offset()..]
        .find('\n')
        .map(|i| i + span.location_offset())
        .unwrap_or(span.extra.len());

    let indent = span.get_utf8_column() - 1;
    format!(
        "{} // Line {}\n{:>indent$}^ {message}",
        &span.extra[beginning_of_line..end_of_line],
        span.location_line(),
        ""
    )
}

impl std::fmt::Display for TypeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DuplicateDeclaration(msg) => write!(f, "{msg}"),
            UndefinedVariable(x) => write!(f, "undefined variable: {x}"),
            TAppRightNotClosed(ty) => write!(
                f,
                "right-hand-side of type application contains free type variables: {ty}"
            ),
            Shadowing(n) => write!(f, "shadowing is disallowed in types: {n}"),
            NotProperlyFormed(n, (sp, ty), ctx) => write!(
                f,
                "{}",
                draw_located_span(
                    sp,
                    format!(
                        "inserting variable '{n} : {}' to failed, type is not properly formed because it contains unbound type variables",
                        ty.to_string_ctx(ctx)
                    )
                )
            ),
            FreeTypeVariable(n) => write!(f, "free type variable {n}"),
            Mismatch((hole, NamedType::Hole), (_, ty), ctx)
            | Mismatch((_, ty), (hole, NamedType::Hole), ctx) => write!(
                f,
                "{}",
                draw_located_span(
                    hole,
                    format!(
                        "Ran into a hole '_', you probably want to fill it in with {}",
                        ty.to_string_ctx(ctx)
                    )
                )
            ),
            Mismatch((span1, ty1), (span2, ty2), ctx) => {
                writeln!(
                    f,
                    "Expected {} but got {}",
                    ty1.to_string_ctx(ctx),
                    ty2.to_string_ctx(ctx)
                )?;
                writeln!(
                    f,
                    "{}",
                    draw_located_span(
                        span2,
                        format!(
                            "This is of type {} while {} was expected",
                            ty2.to_string_ctx(ctx),
                            ty1.to_string_ctx(ctx)
                        )
                    )
                )?;
                writeln!(
                    f,
                    "{}",
                    draw_located_span(
                        span1,
                        format!("Expected {} due to this", ty1.to_string_ctx(ctx))
                    )
                )?;
                match (ty1, ty2) {
                    (NamedType::Arrow(_, _), NamedType::Abs { .. }) => {
                        write!(f, "Hint: Did you mean to apply a type here?")
                    }
                    (NamedType::Abs { .. }, NamedType::Arrow(_, _)) => {
                        write!(f, "Hint: Did you mean to apply a term here?")
                    }
                    _ => Ok(()),
                }
            }
            CantCompare((span1, ty1), (span2, ty2), ctx) => {
                writeln!(
                    f,
                    "Can't compare {} with {}!",
                    ty1.to_string_ctx(ctx),
                    ty2.to_string_ctx(ctx)
                )?;
                writeln!(
                    f,
                    "{}",
                    draw_located_span(span1, format!("This is of type {}", ty1.to_string_ctx(ctx)))
                )?;
                write!(
                    f,
                    "{}",
                    draw_located_span(span2, format!("This is of type {}", ty2.to_string_ctx(ctx)))
                )
            }
            UnknownSyntax(_sp, msg) => write!(f, "unknown syntax: {msg}"), // TODO this should probably be a panic instead
            Holes => write!(
                f,
                "Term contains at least one hole, but we were unable to figure out the corresponding types."
            ),
        }
    }
}

impl std::error::Error for TypeError<'_> {}

// impl From<Token<'_>> for Type {
//     fn from(value: Token<'_>) -> Self {
//         match value {
//             Token::Boolean => todo!(),
//             Token::Integer => todo!(),
//             Token::Character => todo!(),
//             Token::Unit => todo!(),
//             Token::Arrow(ty, ty1) => todo!(),
//             Token::Prod(ty, ty1) => todo!(),
//             Token::List(ty) => todo!(),
//             Token::Sum(ty, ty1) => todo!(),
//             Token::TVar(_) => todo!(),
//             Token::Forall { var, body } => todo!(),
//             Token::Hole => todo!(),
//         }
//     }
// }
