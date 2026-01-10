use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::*,
    combinator::{cut, opt},
    error::ParseError,
    sequence::terminated,
};
use nom_language::precedence::{Assoc, Binary, binary_op, unary_op};

use super::*;
use crate::{
    parse::{Span, ws0_max1_nl_comments},
    term::parse::{parse_term, parse_usize, parse_variable_name},
};

impl Prefix {
    pub fn parse_op<'a, E: ParseError<Span<'a>>>(
        &'_ self,
        input: Span<'a>,
    ) -> IResult<Span<'a>, Unary<(Span<'a>, &'a str), usize>, E> {
        unary_op(
            self.prec,
            (
                ws0_max1_nl_comments,
                position,
                terminated(tag(self.op.0.as_str()), ws0_max1_nl_comments)
                    .map(|op_sym: Span<'a>| *op_sym.fragment()),
            )
                .map(|(_, a, b)| (a, b)),
        )
        .parse(input)
    }
}

impl Infix {
    pub fn parse_op<'a, E: ParseError<Span<'a>>>(
        &'_ self,
        input: Span<'a>,
    ) -> IResult<Span<'a>, Binary<(Span<'a>, &'a str), usize>, E> {
        binary_op(
            self.prec,
            self.assoc,
            (
                ws0_max1_nl_comments,
                position,
                terminated(tag(self.op.0.as_str()), ws0_max1_nl_comments)
                    .map(|op_sym: Span<'a>| *op_sym.fragment()),
            )
                .map(|(_, a, b)| (a, b)),
        )
        .parse(input)
    }
}

/// A character is accepted as a symbol if it's not whitespace, alphanumeric
pub fn is_symbolic(c: char) -> bool {
    !c.is_whitespace() && !c.is_alphanumeric()
}

pub fn parse_operator_symbol<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, String, E> {
    take_while(is_symbolic)
        .map(|s: Span<'a>| s.to_string())
        .parse(input)
}

// TODO add type-level syntax
// pub fn parse_syntax_symbol<'a, E: ParseError<Span<'a>>>(
//     input: Span<'a>,
// ) -> IResult<Span<'a>, SyntaxAtom, E> {
//     verify(anychar, is_symbolic)
//         .map(SyntaxAtom::Symbol)
//         .parse(input)
// }

// pub fn parse_syntax_atom<'a, E: ParseError<Span<'a>>>(
//     input: Span<'a>,
// ) -> IResult<Span<'a>, SyntaxAtom, E> {
//     alt((
//         parse_variable_name.map(SyntaxAtom::Identifier),
//         parse_syntax_symbol::<E>,
//     ))
//     .parse(input)
// }
/// ```stlc
/// prefix ! x = not x
/// prefix:10 - n = 0 - n
/// ```
pub fn parse_infix<
    E: ParseError<Span<'static>>
        + nom::error::ContextError<Span<'static>>
        + 'static
        + nom::error::FromExternalError<Span<'static>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'static>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'static>, E>,
>(
    syntaxes: &'_ Syntaxes,
    input: Span<'static>, // TODO figure out lifetimes
) -> IResult<Span<'static>, Infix, E> {
    (
        tag("infix"),
        cut((
            alt((char('l'), char('r'))).map(|c| if c == 'l' { Assoc::Left } else { Assoc::Right }),
            opt((char(':'), parse_usize).map(|(_, x)| x)).map(|o| o.unwrap_or(5)), // Default precedence level is 5
            space1,
            parse_variable_name,
            space1,
            parse_operator_symbol,
            space1,
            parse_variable_name,
            space0,
            tag("="),
            space0,
            |input| parse_term(syntaxes, input),
        )),
    )
        .map(
            |(_, (assoc, prec, _, lhs, _, op, _, rhs, _, _, _, body))| Infix {
                prec,
                assoc,
                lhs,
                op: Operator(op),
                rhs,
                body,
            },
        )
        .parse(input)
}

/// ```stlc
/// infixr x :: xs = cons x xs
/// infixl:10 xs ++ ys = append xs ys
/// ```
///
/// The right-hand side is parsed as a *term token template* (a `term::parse::SpannedToken`)
/// so we can later desugar surface syntax (e.g. `Token::Infix`) into core tokens *without
/// losing spans* needed for diagnostic locations during type checking.
///
/// The same body is also preserved as a core `Term` (`body_core`) for compatibility with
/// existing utilities/tests, but the desugaring pass should prefer the token template.
pub fn parse_prefix<
    E: ParseError<Span<'static>>
        + nom::error::ContextError<Span<'static>>
        + 'static
        + nom::error::FromExternalError<Span<'static>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'static>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'static>, E>,
>(
    syntaxes: &'_ Syntaxes,
    input: Span<'static>, // TODO figure out lifetimes
) -> IResult<Span<'static>, Prefix, E> {
    (
        tag("prefix"),
        cut((
            opt((char(':'), parse_usize).map(|(_, x)| x)).map(|o| o.unwrap_or(5)), // Default precedence level is 5
            space1,
            parse_operator_symbol,
            space1,
            parse_variable_name,
            space0,
            tag("="),
            space0,
            |input| parse_term(syntaxes, input),
        )),
    )
        .map(|(_, (prec, _, op, _, rhs, _, _, _, body))| Prefix {
            prec,
            op: Operator(op),
            rhs,
            body,
        })
        .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infix() {
        let mut syntaxes = Syntaxes::new();
        let (_, pipe) = parse_infix::<nom_language::error::VerboseError<_>>(
            &syntaxes,
            Span::new_extra("infixl:20 x <| y = x y", ""),
        )
        .unwrap();

        syntaxes.infix.push(pipe);

        let input = Span::new_extra("f <| x // end", "");
        let (rest, _parsed) = parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
            .expect("parse_term failed");
        assert_eq!(*rest.fragment(), " // end");
    }

    #[test]
    fn infix_parses_comments_between_operands() {
        let mut syntaxes = Syntaxes::new();
        let (_, pipe) = parse_infix::<nom_language::error::VerboseError<_>>(
            &syntaxes,
            Span::new_extra("infixl:20 x <| y = x y", ""),
        )
        .unwrap();

        syntaxes.infix.push(pipe);

        let input = Span::new_extra("f // comment\n  <| x // end", "");
        let (rest, _parsed) = parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
            .expect("parse_term failed");
        assert_eq!(*rest.fragment(), " // end");
    }
}
