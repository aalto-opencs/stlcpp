use nom::{
    AsChar, IResult, Input, Offset, Parser,
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{char, space0, space1},
    combinator::{opt, recognize},
    error::{ErrorKind, ParseError},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str, &'a str>;

/// Parse a comment preceded by spaces without consuming the trailing newline.
pub fn parse_comment<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(preceded(
        space0,
        (tag("//"), take_till(|c: char| c.is_newline())),
    ))
    .parse(input)
}

/// Whitespace parser that behaves like `multispace0` but **allows at most one newline** (`\n`).
///
/// It accepts:
/// - any amount of horizontal whitespace (`space0`), and
/// - optionally a single `\n` surrounded by horizontal whitespace.
///
/// Notes:
/// - This intentionally treats **only `\n`** as a newline. If your input may contain `\r\n`,
///   normalize it before parsing.
pub fn ws0_max1_nl<I, E>(input: I) -> IResult<I, I, E>
where
    I: Input + Clone + Offset,
    <I as Input>::Item: AsChar,
    E: ParseError<I>,
{
    // space0 ( '\n' space0 )?
    recognize(pair(
        space0::<I, E>,
        opt(preceded(char('\n'), space0::<I, E>)),
    ))
    .parse(input)
}

/// Whitespace parser that behaves like `multispace1` but **allows at most one newline** (`\n`).
///
/// It accepts:
/// - at least one horizontal whitespace (`space1`), optionally followed by `\n` + `space0`, OR
/// - `space0` + `\n` + `space0` (i.e. a bare newline, optionally surrounded by horizontal whitespace)
///
/// Notes:
/// - This intentionally treats **only `\n`** as a newline. If your input may contain `\r\n`,
///   normalize it before parsing.
pub fn ws1_max1_nl<I, E>(input: I) -> IResult<I, I, E>
where
    I: Input + Clone + Offset,
    <I as Input>::Item: AsChar,
    E: ParseError<I>,
{
    // Branch 1: one or more horizontal whitespace, optionally then a newline block.
    let hspace_then_opt_nl = recognize(pair(
        space1::<I, E>,
        opt(preceded(char('\n'), space0::<I, E>)),
    ));

    // Branch 2: optional horizontal whitespace, then a newline, then optional horizontal whitespace.
    let nl_block = recognize((space0::<I, E>, char::<I, E>('\n'), space0::<I, E>));

    nom::branch::alt((hspace_then_opt_nl, nl_block)).parse(input)
}

/// Whitespace parser that skips comments. New lines that precede comments are not counted.
/// A new line followed by a comment is considered as a comment and parsed.
pub fn ws1_max1_nl_comments<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    // Branch 1: one or more comment blocks (each starts with '\n'), optionally then a newline + spaces.
    let comments_then_opt_nl = recognize((
        space0,
        separated_list1(char('\n'), parse_comment),
        opt(preceded(char('\n'), space0)),
    ));

    // Branch 2: new line, then one or more comment blocks
    let nl_then_comments_then_opt_nl = recognize((
        space0,
        char('\n'),
        separated_list1(char('\n'), parse_comment),
        opt(preceded(char('\n'), space0)),
    ));

    // Branch 3: zero or more comment blocks, then a newline block (space0 '\n' space0).
    let opt_comments_then_nl_block = recognize((
        separated_list0(char('\n'), parse_comment),
        space0,
        char('\n'),
        space0,
    ));

    alt((
        comments_then_opt_nl,
        nl_then_comments_then_opt_nl,
        opt_comments_then_nl_block,
        // Branch 4: one or more spaces
        space1,
    ))
    .parse(input)
}

pub fn ws0_max1_nl_comments<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    alt((ws1_max1_nl_comments, space0)).parse(input)
}

#[deprecated = "Use ws0_comments instead"]
/// Wrap a parser with optional whitespace (max one newline) on both sides.
pub fn ws0<I, F, O, E>(inner: F) -> impl Parser<I, Output = O, Error = E>
where
    F: Parser<I, Output = O, Error = E>,
    E: ParseError<I>,
    I: Input + Clone + Offset,
    <I as Input>::Item: AsChar,
{
    // ws0_max1_nl returns the consumed slice; we ignore it.
    preceded(ws0_max1_nl, terminated(inner, ws0_max1_nl))
}

#[deprecated = "Use ws1_comments instead"]
/// Wrap a parser with required whitespace (max one newline) on both sides.
pub fn ws1<I, F, O, E>(inner: F) -> impl Parser<I, Output = O, Error = E>
where
    F: Parser<I, Output = O, Error = E>,
    E: ParseError<I>,
    I: Input + Clone + Offset,
    <I as Input>::Item: AsChar,
{
    preceded(ws1_max1_nl, terminated(inner, ws1_max1_nl))
}

pub fn ws1_comments<'a, F, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl Parser<Span<'a>, Output = O, Error = E>
where
    F: Parser<Span<'a>, Output = O, Error = E>,
{
    delimited(ws1_max1_nl_comments, inner, ws1_max1_nl_comments)
}

pub fn ws0_comments<'a, F, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl Parser<Span<'a>, Output = O, Error = E>
where
    F: Parser<Span<'a>, Output = O, Error = E>,
{
    delimited(ws0_max1_nl_comments, inner, ws0_max1_nl_comments)
}

/// Try all parsers and return the first successful one (or the first non-recoverable error).
pub fn alt_iter<I, O, E, P, It>(input: I, parsers: It) -> IResult<I, O, E>
where
    I: Clone,
    E: ParseError<I>,
    P: FnMut(I) -> IResult<I, O, E>,
    It: IntoIterator<Item = P>,
{
    let mut last_err: Option<E> = None;

    for mut parser in parsers {
        match parser(input.clone()) {
            // Successfully parsed or non-recoverable error -> return immediately,
            // this mirrors Choice: any non-`Error` result is returned.
            res @ Ok(_) => return res,
            res @ Err(nom::Err::Incomplete(_)) => return res,
            res @ Err(nom::Err::Failure(_)) => return res,

            // Recoverable error: accumulate / combine
            Err(nom::Err::Error(e)) => {
                last_err = match last_err {
                    None => Some(e),
                    Some(prev) => Some(prev.or(e)),
                }
            }
        }
    }

    // All parsers returned Err::Error -> return combined error (or a default)
    match last_err {
        Some(e) => Err(nom::Err::Error(e)),
        None => Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Alt))),
    }
}

/// Try all parsers and return the result that consumed the most input.
/// - If none match: returns an `Err::Error(...)`-like behavior, but without needing ErrorKind;
///   we just return the "best" Error we saw (or a generic one).
/// - If any parser returns `Failure` or `Incomplete`, we return it immediately (like `alt`).
pub fn alt_iter_longest<I, O, E, P, It>(input: I, parsers: It) -> IResult<I, O, E>
where
    I: Clone + Offset,
    E: ParseError<I>,
    P: FnMut(I) -> IResult<I, O, E>,
    It: IntoIterator<Item = P>,
{
    let mut best_ok: Option<(I, O, usize)> = None;
    let mut best_err: Option<E> = None;

    for mut p in parsers.into_iter() {
        match p(input.clone()) {
            Ok((rest, out)) => {
                let consumed = input.offset(&rest);
                match &best_ok {
                    None => best_ok = Some((rest, out, consumed)),
                    Some((_best_rest, _best_out, best_consumed)) => {
                        if consumed > *best_consumed {
                            best_ok = Some((rest, out, consumed));
                        }
                    }
                }
            }

            Err(nom::Err::Incomplete(n)) => return Err(nom::Err::Incomplete(n)),
            Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),

            Err(nom::Err::Error(e)) => {
                best_err = match best_err {
                    None => Some(e),
                    Some(prev) => Some(prev.or(e)),
                }
            }
        }
    }

    if let Some((rest, out, _consumed)) = best_ok {
        Ok((rest, out))
    } else {
        match best_err {
            Some(e) => Err(nom::Err::Error(e)),
            None => Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Alt))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ws1_max1_nl_comments_basic() {
        let input = Span::new_extra("  abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra(" \n abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");
    }

    #[test]
    fn test_ws1_max1_nl_comments() {
        let input = Span::new_extra("\n// comment\n  abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra("    \n// comment\n  abcd", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abcd");

        let input = Span::new_extra("   \n// comment\n\n  abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "\n  abc");

        let input = Span::new_extra("   \n  \n// comment\n  abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "\n// comment\n  abc");

        let input = Span::new_extra("   \n // c1 \n// c2 \n  abc", "");
        let (rest, _parsed) =
            ws1_max1_nl_comments::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");
    }
}
