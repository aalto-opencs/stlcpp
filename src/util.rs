//! Crate-level utilities intended primarily for tests and doctests.
//!
//! The core type checker is implemented for desugared `term::tokens::SpannedToken<Desugared>`.
//! In tests/doctests you often start from a source string and want to:
//! 1) parse a surface term (`parse_term`),
//! 2) desugar custom syntax / infix sugar (`desugar_term`),
//! 3) run type inference / checking.
//!
//! This module provides small, ergonomic helpers for that workflow.

use crate::context::Context;
use crate::syntax::Syntaxes;

use crate::{
    parse::Span,
    term::{
        parse::parse_term,
        tokens::{Desugared, SpannedToken},
    },
    r#type::{TypeError, named_type::NamedType},
};

/// Parse a term from `src` using `syntaxes`, returning the *surface* token tree.
///
/// This intentionally does **not** enforce EOF. If you need EOF, use
/// [`parse_surface_term_eof`].
pub fn parse_surface_term<'a>(
    syntaxes: &Syntaxes,
    src: &'a str,
) -> Result<crate::term::tokens::SpannedToken<'a, crate::term::tokens::Surface>, String> {
    parse_term::<nom::error::Error<Span<'a>>>(syntaxes, Span::new_extra(src, src))
        .map(|(_rest, t)| t)
        .map_err(|e| format!("parse_term failed: {e:?}"))
}

/// Parse a term and require that it consumes the entire input (ignoring surrounding comments/whitespace
/// per the parser’s own policy).
pub fn parse_surface_term_eof<'a>(
    syntaxes: &Syntaxes,
    src: &'a str,
) -> Result<crate::term::tokens::SpannedToken<'a, crate::term::tokens::Surface>, String> {
    let (rest, t) = parse_term::<nom::error::Error<Span<'a>>>(syntaxes, Span::new_extra(src, src))
        .map_err(|e| format!("parse_term failed: {e:?}"))?;

    if !rest.fragment().is_empty() {
        return Err(format!(
            "did not consume full input; remaining: {:?}",
            *rest.fragment()
        ));
    }

    Ok(t)
}

/// Parse + desugar a term from `src` using `syntaxes`.
///
/// This is the most common entrypoint for type-checking tests.
pub fn parse_desugar_term<'a>(
    syntaxes: &Syntaxes,
    src: &'a str,
) -> Result<SpannedToken<'a, Desugared>, String> {
    let surface = parse_surface_term_eof(syntaxes, src)?;
    surface
        .desugar(syntaxes)
        .map_err(|e| format!("desugar_term failed: {e}"))
}

/// Infer the type of a source term string.
///
/// This uses `Context::default()` by default.
pub fn infer_type_src<'a>(syntaxes: &Syntaxes, src: &'a str) -> Result<NamedType, TypeError<'a>> {
    infer_type_src_with_ctx(syntaxes, src, Context::default())
}

/// Infer the type of a source term string in the provided context.
pub fn infer_type_src_with_ctx<'a>(
    syntaxes: &Syntaxes,
    src: &'a str,
    ctx: Context,
) -> Result<NamedType, TypeError<'a>> {
    let t = parse_desugar_term(syntaxes, src)
        .map_err(|msg| TypeError::UnknownSyntax(Span::new_extra(src, src), msg))?;

    // These helpers operate on a source term only (not a module), so we don’t have any
    // module-level type declarations available. Use an empty type-alias environment.
    let aliases = std::collections::HashMap::new();

    t.infer_type(ctx, &aliases)
}

/// Like [`infer_type_src`], but returns a user-friendly error string.
///
/// Helpful in doctests where you don’t want to pattern-match on `TypeError` (which does not implement
/// `PartialEq`).
pub fn infer_type_src_string(syntaxes: &Syntaxes, src: &str) -> Result<String, String> {
    let t = parse_desugar_term(syntaxes, src)?;

    // These helpers operate on a source term only (not a module), so we don’t have any
    // module-level type declarations available. Use an empty type-alias environment.
    let aliases = std::collections::HashMap::new();

    t.infer_type(Context::default(), &aliases)
        .map(|ty| ty.to_string())
        .map_err(|e| e.to_string())
}
