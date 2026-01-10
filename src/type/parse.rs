use nom::combinator::{cut, fail};
use nom::error::{ContextError, ParseError};
use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    character::complete::{alpha1, alphanumeric0, char as character},
    combinator::value,
    multi::many0,
    sequence::delimited,
};
use nom_language::precedence::{Assoc, Operation, binary_op, precedence};
use nom_locate::position;

use super::super::parse::*;
use crate::RESERVED_KEYWORDS;
use crate::r#type::tokens::{SpannedToken, Token};

use Token::*;

pub fn parse_base_type<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        alt((
            value(Boolean, tag("Boolean")),
            value(Integer, tag("Integer")),
            value(Character, tag("Character")),
            value(Unit, tag("Unit")),
        )),
    )
        .map(SpannedToken::new)
        .parse(input)
}

pub fn parse_type_variable_name<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, String, E> {
    // Parse candidate first, then validate so we can attach a targeted context message.
    let mut ident = (alpha1, alphanumeric0).map(|(s1, s2)| format!("{s1}{s2}"));

    let (rest, name) = ident.parse(input)?;

    let first_is_uppercase = name
        .chars()
        .next()
        .expect("BUG: name must be non-empty")
        .is_uppercase();

    if !first_is_uppercase {
        // Fail at the already-parsed identifier span (rest), and attach context there.
        // This avoids re-parsing from `input`, which can interfere with other alternatives.
        let err = E::add_context(
            rest,
            "type variable name must start with an uppercase letter",
            E::from_error_kind(rest, nom::error::ErrorKind::Fail),
        );
        return Err(nom::Err::Error(err));
    }

    if RESERVED_KEYWORDS.contains(&name.as_str()) {
        let err = E::add_context(
            rest,
            "type variable name is a reserved keyword",
            E::from_error_kind(rest, nom::error::ErrorKind::Fail),
        );
        return Err(nom::Err::Error(err));
    }

    Ok((rest, name))
}

fn parse_tvar<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (position, parse_type_variable_name.map(TVar))
        .map(SpannedToken::new)
        .parse(input)
}

pub fn parse_sort<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, (), E> {
    tag("Type").map(|_| ()).parse(input)
}

fn parse_forall_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("forall"),
            cut((
                ws1_max1_nl,
                parse_type_variable_name,
                ws0_max1_nl,
                character(','),
                ws0_max1_nl,
                parse_type,
            )),
        )
            .map(|(_, (_, var, _, _, _, body))| Forall {
                var,
                body: Box::new(body),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_paren_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    // Can't add a cut here because it's not unique with prod
    delimited(char('('), ws0(parse_type), char(')')).parse(input)
}

fn parse_list_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (tag("List"), cut((ws1_max1_nl, parse_type_primary))) // Only base or paren allowed
            .map(|(_, (_, ty))| List(Box::new(ty))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_io_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (tag("IO"), cut((ws1_max1_nl, parse_type_primary))) // Only base or paren allowed
            .map(|(_, (_, ty))| IO(Box::new(ty))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_list_type_brackets<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        // No cut here because List syntax sugar uses the same syntax
        ((char('['), (ws0(parse_type), char(']')))).map(|(_, (ty, _))| List(ty.into())),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn op_ctor<'a>(op: &str) -> fn(Box<SpannedToken<'a>>, Box<SpannedToken<'a>>) -> Token<'a> {
    match op {
        "+" => Sum,
        "->" => Arrow,
        _ => panic!("BUG: unknown operator"),
    }
}

/// Parses an operator.
///
/// `A -> B + C -> D + E` is parsed as `A -> ((B + C) -> (D + E))`
///
/// ```
/// use stlcpp::r#type::parse::*;
/// use stlcpp::r#type::util::*;
/// use stlcpp::parse::*;
/// use stlcpp::r#type::named_type::NamedType::*;
/// assert_eq!(
///     parse_op::<nom::error::Error<_>>(Span::new_extra("Integer -> Integer + Integer -> Integer + Integer", ""))
///        .unwrap().1.token.to_named_type_ctx_with_aliases(vec![], &Default::default()).unwrap(),
///     arrow(Integer, arrow(sum(Integer, Integer), sum(Integer, Integer)))
/// );
/// ```
///
/// Having a List type with brackets [] should have higher precedence than the arrow
///
/// ```
/// use stlcpp::r#type::parse::*;
/// use stlcpp::r#type::util::*;
/// use stlcpp::parse::*;
/// use stlcpp::r#type::named_type::NamedType::*;
/// assert_eq!(
///     parse_op::<nom::error::Error<_>>(Span::new_extra("[Integer] -> Integer", ""))
///         .unwrap().1.token.to_named_type_ctx_with_aliases(vec![], &Default::default()).unwrap(),
///     arrow(list(Integer), Integer)
/// );
/// ```
pub fn parse_op<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + 'a + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    precedence::<_, _, _, _, _, _, _, _, _, (), (), _, _>(
        fail(),
        fail(),
        alt((
            binary_op(30, Assoc::Left, ws0(tag("+"))),
            binary_op(40, Assoc::Right, ws0(tag("->"))),
        )),
        parse_type_primary,
        |op: Operation<_, _, _, SpannedToken>| {
            use nom_language::precedence::Operation::*;
            match op {
                Binary(lhs, op, rhs) => Ok(SpannedToken::new((
                    lhs.position,
                    op_ctor(&op)(Box::new(lhs), Box::new(rhs)),
                ))),
                _ => panic!("Invalid combination"),
            }
        },
    )
    .parse(input)
}

fn parse_prod_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        ((
            char('('),
            ws0(parse_type),
            char(','),
            cut((ws0(parse_type), char(')'))),
        ))
            .map(|(_, ty1, _, (ty2, _))| Prod(ty1.into(), ty2.into())),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_hole_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (position, value(Hole, tag("_")))
        .map(SpannedToken::new)
        .parse(input)
}

/// Primary (always consuming) parser for types.
pub fn parse_type_primary<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    alt((
        parse_base_type,
        parse_prod_type,
        parse_paren_type,
        parse_tvar,
        parse_list_type_brackets, // TODO custom syntax
        parse_hole_type,
        parse_list_type,
        parse_io_type,
    ))
    .parse(input)
}

pub fn parse_type<
    'a,
    E: ParseError<Span<'a>> + ContextError<Span<'a>> + nom::error::FromExternalError<Span<'a>, E> + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    delimited(
        many0(ws0(parse_comment)),
        alt((parse_forall_type, parse_op)),
        many0(ws0(parse_comment)),
    )
    .parse(input)
}
