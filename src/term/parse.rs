use std::collections::VecDeque;
use std::marker::PhantomData;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{
        complete::{tag, take_while, take_while_m_n},
        take_till,
    },
    character::{
        anychar,
        complete::{alpha1, char as character, digit1},
    },
    combinator::{consumed, cut, fail, opt, value, verify},
    error::{ContextError, ParseError},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, preceded},
};
use nom_language::precedence::{Operation, precedence};
use nom_locate::position;

use crate::r#type::parse::{
    SpannedToken as TSpannedToken, Token as TToken, parse_sort, parse_type_primary,
    parse_type_variable_name,
};
use crate::{RESERVED_KEYWORDS, r#type::parse::parse_type};
use crate::{parse::*, syntax::Syntaxes};

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Token<'a, S = Surface> {
    /// A variable term, represented by a string.
    Var(String),
    /// An abstraction (lambda function) with a variable and a body term.
    Abs {
        var: String,
        ty: TSpannedToken<'a>,
        body: Box<SpannedToken<'a, S>>,
    },
    /// An application of one term to another.
    App(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),

    /// A let expression assigning a variable `var` to a value `val_t` in `body`.
    /// It is effectively just a subtitution.
    Let {
        var: String,
        val_t: Box<SpannedToken<'a, S>>,
        body: Box<SpannedToken<'a, S>>,
    },

    /// A true boolean value
    True,
    /// A false boolean value
    False,
    /// If-then-else
    Ite {
        cond: Box<SpannedToken<'a, S>>,
        if_true: Box<SpannedToken<'a, S>>,
        if_false: Box<SpannedToken<'a, S>>,
    },

    /// An integer value
    Int(num_bigint::BigInt),
    /// Addition of two terms
    Add(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Subtraction of two terms
    Sub(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Multiplication of two terms
    Mul(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Division of two terms
    Div(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),

    /// Equality comparison
    Eq(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Non-equality
    Ne(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Less than
    Lt(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Less than or equal
    Le(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Greater than
    Gt(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Greater than or equal
    Ge(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),

    /// A unicode character/code-point
    Char(char),

    /// The element of Unit
    Trivial,

    /// A pair consisting of terms
    Pair(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// The first term in the pair
    Fst(Box<SpannedToken<'a, S>>),
    /// The second term in the pair
    Snd(Box<SpannedToken<'a, S>>),

    /// An empty list of some item type
    Nil(TSpannedToken<'a>),
    /// The recursive constructor for lists, holds the head and the tail in the following order: `Cons(head, tail)`.
    Cons(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),
    /// Case analysis for lists
    ///
    /// ```text
    /// lcase t of
    /// | nil => nil_t
    /// | cons cons_var tail_var => cons_t
    /// ```
    LCase {
        t: Box<SpannedToken<'a, S>>,
        nil_t: Box<SpannedToken<'a, S>>,
        head_var: String,
        tail_var: String,
        cons_t: Box<SpannedToken<'a, S>>,
    },

    /// Injection to the left with the type of the right
    Inl(Box<SpannedToken<'a, S>>, TSpannedToken<'a>),
    /// Injection to the right with the type of the left
    Inr(Box<SpannedToken<'a, S>>, TSpannedToken<'a>),
    /// Case analysis for sum types
    ///
    /// ```text
    /// lcase t of
    /// | inl inl_var => inl_t
    /// | inr inr_var => inr_t
    /// ```
    Case {
        t: Box<SpannedToken<'a, S>>,
        inl_var: String,
        inl_t: Box<SpannedToken<'a, S>>,
        inr_var: String,
        inr_t: Box<SpannedToken<'a, S>>,
    },

    /// Fixed point combinator.
    /// Calculates the fixed point of the inner function.
    Fix(Box<SpannedToken<'a, S>>),

    /// A polymorphic abstraction
    TAbs {
        var: String,
        body: Box<SpannedToken<'a, S>>,
    },
    /// Application of a lambda term to a type
    TApp(Box<SpannedToken<'a, S>>, TSpannedToken<'a>),

    /// A panic term. Evaluating this prints an error and as a side-effect stops further evaluation
    Panic(bool, TSpannedToken<'a>, Box<SpannedToken<'a, S>>),

    Trace(usize, Box<SpannedToken<'a, S>>), // --> Term (side-effect: "trace: {term}")
    // Prefix(Operator, String, Term),
    // Postfix(String, Operator, Term),
    Print(Box<SpannedToken<'a, S>>),
    ReadLine,
    IOPure(TSpannedToken<'a>, Box<SpannedToken<'a, S>>),
    IOBind {
        dom: TSpannedToken<'a>,
        cod: TSpannedToken<'a>,
        func: Box<SpannedToken<'a, S>>,
        t: Box<SpannedToken<'a, S>>,
    },

    /// A surface-syntax infix application produced by custom syntax parsing.
    ///
    /// This is intended to be desugared later (after parsing) into a core term.
    /// `op` is stored as the operator symbol as it appeared in the source.
    ///
    /// Note: This variant is only valid for `S = Surface`.
    Infix {
        op: &'a str,
        lhs: Box<SpannedToken<'a, S>>,
        rhs: Box<SpannedToken<'a, S>>,
    },

    /// A surface-syntax prefix operator produced by custom syntax parsing.
    ///
    /// This is intended to be desugared later (after parsing) into a core term.
    /// `op` is stored as the operator symbol as it appeared in the source.
    ///
    /// Note: This variant is only valid for `S = Surface`.
    Prefix {
        op: &'a str,
        rhs: Box<SpannedToken<'a, S>>,
    },
}

use Token::*;

#[derive(Debug, Clone)]
pub struct SpannedToken<'a, S = Surface> {
    pub token: Token<'a, S>,
    pub position: Span<'a>,
    pub(crate) _state: PhantomData<S>,
}

pub type SurfaceSpannedToken<'a> = SpannedToken<'a, Surface>;
pub type DesugaredSpannedToken<'a> = SpannedToken<'a, Desugared>;

impl<'a, S> SpannedToken<'a, S> {
    pub fn new((position, token): (Span<'a>, Token<'a, S>)) -> Self {
        Self {
            token,
            position,
            _state: PhantomData,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Surface;

#[derive(Debug, Clone, Copy)]
pub struct Desugared;

pub fn variable_pred(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '.' || c == '\''
}

pub fn parse_variable_name<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, String, E> {
    // Parse candidate identifier first, then validate.
    let mut ident = (alpha1, take_while(variable_pred)).map(|(s1, s2)| format!("{s1}{s2}"));

    let (rest, name) = ident.parse(input)?;

    let first_char = name.chars().next().expect("BUG: name must be non-empty");

    // Term variables must start with a lowercase letter.
    // It must be distinct from how types are parsed (notably must not allow _), because TApp parser parses a sequence of terms or types.
    if !first_char.is_lowercase() {
        return Err(nom::Err::Error(E::from_error_kind(
            input,
            nom::error::ErrorKind::Verify,
        )));
    }

    if RESERVED_KEYWORDS.contains(&name.as_str()) {
        let err = E::add_context(
            rest,
            "variable name is a reserved keyword",
            E::from_error_kind(rest, nom::error::ErrorKind::Fail),
        );
        return Err(nom::Err::Error(err));
    }

    Ok((rest, name))
}

/// Use this where the variable name is allowed to be an underscore
pub fn parse_variable_name_special<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, String, E> {
    alt((character('_').map(|c| c.to_string()), parse_variable_name)).parse(input)
}

fn parse_var<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    consumed(parse_variable_name.map(Var))
        .map(SpannedToken::new)
        .parse(input)
}

// Source: https://github.com/rust-bakery/nom/blob/main/examples/string.rs

use nom::AsChar;

fn parse_delimited_hex<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, char, E> {
    let parse_hex = take_while_m_n::<_, Span<'a>, E>(1, 6, AsChar::is_hex_digit);

    let parse_delimited_hex = preceded(
        character('u'),
        delimited(character('{'), parse_hex, character('}')),
    );

    parse_delimited_hex
        .map_res(|hex| u32::from_str_radix(hex.fragment(), 16))
        .map_opt(std::char::from_u32)
        .parse(input)
}

/// Parse an escaped character.
///
/// Supports escaped unicode characters of the form \u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals, like \u{00AC}.
fn parse_escaped_char<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (
        position,
        preceded(
            character('\\'),
            alt((
                parse_delimited_hex,
                // map_opt(parse_u32, |c| std::char::from_u32(c)).parse(input),
                // The `value` parser returns a fixed value (the first argument) if its
                // parser (the second argument) succeeds. In these cases, it looks for
                // the marker characters (n, r, t, etc) and returns the matching
                // character (\n, \r, \t, etc).
                value('\n', character('n')),
                value('\r', character('r')),
                value('\t', character('t')),
                value('\u{08}', character('b')),
                value('\u{0C}', character('f')),
                value('\\', character('\\')),
                value('/', character('/')),
                value('"', character('"')),
            )),
        )
        .map(Token::Char),
    )
        .map(SpannedToken::new)
        .parse(input)
}

/// Parses individual characters from a char literal
fn parse_char_literal<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (
        position,
        verify(anychar, |s: &char| *s != '\\' && *s != '\'').map(Token::Char),
    )
        .map(SpannedToken::new)
        .parse(input)
}

/// Parses individual characters from a string literal
fn parse_string_literal<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (
        position,
        verify(anychar, |s: &char| *s != '\\' && *s != '\"').map(Token::Char),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_char<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (
        character('\''),
        cut((
            alt((parse_escaped_char, parse_char_literal)),
            character('\''),
        )),
    )
        .map(|(_, (c, _))| c)
        .parse(input)
}

/// Parses a string literal as a list of characters
fn parse_string_as_list_char<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    let (input, pos) = position(input)?;

    let build_string = fold_many0(
        alt((parse_escaped_char, parse_string_literal)),
        VecDeque::new,
        |mut lhs, c| {
            lhs.push_front(c);
            lhs
        },
    );

    fn fold_vecdeque_to_list_char<'a>(
        v: VecDeque<SurfaceSpannedToken<'a>>,
        pos: Span<'a>,
    ) -> SurfaceSpannedToken<'a> {
        v.into_iter().fold(
            SpannedToken {
                position: pos,
                token: Token::Nil(TSpannedToken {
                    token: TToken::Character,
                    position: pos, // TODO is this correct?
                }),
                _state: PhantomData,
            },
            |lhs, c| SpannedToken {
                position: c.position,
                token: Token::Cons(Box::new(c), Box::new(lhs)),
                _state: PhantomData,
            },
        )
    }

    (
        character('"'),
        cut((
            build_string.map(|v| fold_vecdeque_to_list_char(v, pos)),
            character('"'),
        )),
    )
        .map(|(_, (s, _))| s)
        .parse(input)
}

fn parse_unit<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (position, tag("()").map(|_| Token::Trivial))
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_abs<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("fun"),
            ws1_max1_nl,
            parse_variable_name_special,
            ws0_max1_nl,
            character(':'),
            ws0_max1_nl,
            parse_type,
            cut((ws0_max1_nl, character(','), ws0_max1_nl, |input| {
                parse_term(syntaxes, input)
            })),
        )
            .map(|(_, _, var, _, _, _, ty, (_, _, _, body))| Abs {
                var,
                ty,
                body: body.into(),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_tabs<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("fun"),
            ws1_max1_nl,
            parse_type_variable_name,
            ws0_max1_nl,
            opt((character(':'), ws0_max1_nl, parse_sort, ws0_max1_nl)),
            cut((character(','), ws0_max1_nl, |input| {
                parse_term(syntaxes, input)
            })),
        )
            .map(|(_, _, var, _, _o, (_, _, body))| TAbs {
                var,
                body: body.into(),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_paren<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    delimited(
        character('('),
        ws0(|input| parse_term(syntaxes, input)),
        character(')'),
    )
    .parse(input)
}

/// Parses an application of n terms
pub fn parse_app<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    let (rest, t1) = (|input| parse_term_primary(syntaxes, input)).parse(input)?;

    fold_many0(
        (ws1_max1_nl, (|input| parse_term_or_type(syntaxes, input))),
        move || t1.clone(),
        |fun, (_, arg)| match arg {
            Either::Left(t) => SpannedToken {
                position: fun.position, // TODO is this correct?
                token: App(Box::new(fun), Box::new(t)),
                _state: PhantomData,
            },
            Either::Right(ty) => SpannedToken {
                position: fun.position,
                token: TApp(Box::new(fun), ty),
                _state: PhantomData,
            },
        },
    )
    .parse(rest)
}

#[derive(Debug)]
pub enum Either<T1, T2> {
    Left(T1),
    Right(T2),
}

pub fn parse_term_or_type<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, Either<SurfaceSpannedToken<'a>, TSpannedToken<'a>>, E> {
    alt((
        (|input| parse_term_primary(syntaxes, input)).map(|t| Either::Left(t)),
        // IMPORTANT: type parsing must use the same error type `E` as term parsing here,
        // otherwise type applications inside application chains can spuriously fail.
        parse_type_primary::<E>.map(|t| Either::Right(t)),
    ))
    .parse(input)
}

pub fn parse_int<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    (
        position,
        digit1::<Span<'a>, E>
            .map_res(|s| s.fragment().parse())
            .map(Int),
    )
        .map(SpannedToken::new)
        .parse(input)
}

pub fn parse_usize<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, usize, E> {
    digit1::<Span<'a>, E>
        .map_res(|s| s.fragment().parse())
        .parse(input)
}

fn parse_bool<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        alt((value(True, tag("true")), value(False, tag("false")))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_ite<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("if"),
            cut((
                ws1(|input| parse_term(syntaxes, input)),
                tag("then"),
                ws1(|input| parse_term(syntaxes, input)),
                tag("else"),
                ws1_max1_nl,
                |input| parse_term(syntaxes, input),
            )),
        )
            .map(|(_, (cond, _, if_true, _, _, if_false))| Ite {
                cond: cond.into(),
                if_true: if_true.into(),
                if_false: if_false.into(),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_pair<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        delimited(
            character('('),
            (
                ws0(|input| parse_term(syntaxes, input)),
                character(','),
                cut(ws0(|input| parse_term(syntaxes, input))),
            ),
            character(')'),
        )
        .map(|(t1, _, t2)| Pair(t1.into(), t2.into())),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_fst_snd<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position, // TODO is this correct
        (
            alt((tag("fst"), tag("snd"))),
            cut((ws1_max1_nl, |input| parse_term_primary(syntaxes, input))),
        )
            .map(|(op, (_, t))| match op.as_ref() {
                "fst" => Fst(t.into()),
                "snd" => Snd(t.into()),
                _ => unreachable!("BUG"),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_nil<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (tag("nil"), cut((ws1_max1_nl, parse_type_primary))).map(|(_, (_, ty))| Nil(ty.into())),
    )
        .map(SpannedToken::new)
        .parse(input)
}

macro_rules! make_binary_parser {
    ($name:ident, $tag:literal, $term:ident) => {
        fn $name<
            'a,
            'b,
            E: ParseError<Span<'a>>
                + ContextError<Span<'a>>
                + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
                + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
                + nom::error::FromExternalError<Span<'a>, E>
                + 'a,
        >(
            syntaxes: &'b Syntaxes,
            input: Span<'a>,
        ) -> IResult<Span<'a>, SpannedToken<'a>, E> {
            (
                position,
                (
                    tag($tag),
                    cut((
                        ws1_max1_nl,
                        |input| parse_term_primary(syntaxes, input),
                        ws1_max1_nl,
                        |input| parse_term_primary(syntaxes, input),
                    )),
                )
                    .map(|(_op, (_, lhs, _, rhs))| $term(lhs.into(), rhs.into())),
            )
                .map(SpannedToken::new)
                .parse(input)
        }
    };
}

make_binary_parser!(parse_cons, "cons", Cons);
make_binary_parser!(parse_add, "__add", Add);
make_binary_parser!(parse_sub, "__sub", Sub);
make_binary_parser!(parse_mul, "__mul", Mul);
make_binary_parser!(parse_div, "__div", Div);
make_binary_parser!(parse_ge, "__ge", Ge);
make_binary_parser!(parse_le, "__le", Le);
make_binary_parser!(parse_eq, "__eq", Eq);
make_binary_parser!(parse_ne, "__ne", Ne);
make_binary_parser!(parse_gt, "__gt", Gt);
make_binary_parser!(parse_lt, "__lt", Lt);

fn parse_binary_ops<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    alt((
        |input| parse_add(syntaxes, input),
        |input| parse_sub(syntaxes, input),
        |input| parse_mul(syntaxes, input),
        |input| parse_div(syntaxes, input),
        |input| parse_ge(syntaxes, input),
        |input| parse_le(syntaxes, input),
        |input| parse_eq(syntaxes, input),
        |input| parse_ne(syntaxes, input),
        |input| parse_gt(syntaxes, input),
        |input| parse_lt(syntaxes, input),
    ))
    .parse(input)
}

/// Primary (always consuming) parser for terms.

fn parse_list<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        character('['),
        ws0_max1_nl,
        separated_list0(ws0(character(',')), |input| parse_term(syntaxes, input)),
        ws0(character(':')),
        cut((parse_type, ws0_max1_nl, character(']'))),
    )
        .map(|(pos, _, _, terms, _, (ty, _, _))| {
            let mut list = SpannedToken {
                token: Nil(ty),
                position: pos, // TODO Is this correct?
                _state: PhantomData,
            };
            for t in terms.into_iter().rev() {
                list = SpannedToken {
                    token: Cons(Box::new(t), Box::new(list)),
                    position: pos, // TODO Is this correct?
                    _state: PhantomData,
                }
            }
            list
        })
        // .map(SpannedToken::new)
        .parse(input)
}

fn parse_lcase<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    let arm_nil = (
        tag("|"),
        ws0_max1_nl,
        tag("nil"),
        ws0_max1_nl,
        tag("=>"),
        ws0_max1_nl,
        |input| parse_term(syntaxes, input),
    )
        .map(|(_, _, _, _, _, _, t)| t);
    let arm_cons = (
        tag("|"),
        ws0_max1_nl,
        tag("cons"),
        ws1_max1_nl,
        parse_variable_name_special,
        ws1_max1_nl,
        parse_variable_name_special,
        ws0_max1_nl,
        tag("=>"),
        ws0_max1_nl,
        |input| parse_term(syntaxes, input),
    )
        .map(|(_, _, _, _, head_var, _, tail_var, _, _, _, cons_t)| (head_var, tail_var, cons_t));

    (
        position,
        (
            tag("lcase"),
            cut((
                ws1_max1_nl,
                |input| parse_term(syntaxes, input),
                ws1_max1_nl,
                tag("of"),
                ws0_max1_nl,
                arm_nil,
                ws0_max1_nl,
                arm_cons,
            )),
        )
            .map(
                |(_, (_, t, _, _, _, nil_t, _, (head_var, tail_var, cons_t)))| LCase {
                    t: t.into(),
                    nil_t: nil_t.into(),
                    head_var,
                    tail_var,
                    cons_t: cons_t.into(),
                },
            ),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_inl_inr<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            alt((tag("inl"), tag("inr"))),
            cut((
                ws1_max1_nl,
                |input| parse_term_primary(syntaxes, input),
                ws1_max1_nl,
                parse_type_primary,
            )),
        )
            .map(|(op, (_, t, _, ty))| match op.as_ref() {
                "inl" => Inl(t.into(), ty),
                "inr" => Inr(t.into(), ty),
                _ => unreachable!("BUG"),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_case<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    let arm_l = (
        tag("|"),
        ws0_max1_nl,
        tag("inl"),
        ws1_max1_nl,
        parse_variable_name_special,
        ws0_max1_nl,
        tag("=>"),
        ws0_max1_nl,
        |input| parse_term(syntaxes, input),
    )
        .map(|(_, _, _, _, inl_var, _, _, _, inl_t)| (inl_var, inl_t));
    let arm_r = (
        tag("|"),
        ws0_max1_nl,
        tag("inr"),
        ws1_max1_nl,
        parse_variable_name_special,
        ws0_max1_nl,
        tag("=>"),
        ws0_max1_nl,
        |input| parse_term(syntaxes, input),
    )
        .map(|(_, _, _, _, inr_var, _, _, _, inr_t)| (inr_var, inr_t));
    (
        position,
        (
            tag("case"),
            cut((
                ws1_max1_nl,
                |input| parse_term(syntaxes, input),
                ws1_max1_nl,
                tag("of"),
                ws0_max1_nl,
                arm_l,
                ws0_max1_nl,
                arm_r,
            )),
        )
            .map(
                |(_, (_, t, _, _, _, (inl_var, inl_t), _, (inr_var, inr_t)))| Case {
                    t: t.into(),
                    inl_var,
                    inl_t: inl_t.into(),
                    inr_var,
                    inr_t: inr_t.into(),
                },
            ),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_let<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("let"),
            cut((
                ws1_max1_nl,
                parse_variable_name_special,
                ws0_max1_nl,
                tag("="),
                ws0_max1_nl,
                |input| parse_term(syntaxes, input),
                ws1_max1_nl,
                tag("in"),
                ws1_max1_nl,
                |input| parse_term(syntaxes, input),
            )),
        )
            .map(|(_, (_, var, _, _, _, val_t, _, _, _, body))| Let {
                var,
                val_t: val_t.into(),
                body: body.into(),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_fix<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("fix"),
            cut((ws1_max1_nl, |input| parse_term_primary(syntaxes, input))),
        )
            .map(|(_, (_, t))| Fix(Box::new(t))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_panic<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("panic"),
            cut((ws1_max1_nl, parse_type, ws1_max1_nl, |input| {
                parse_term(syntaxes, input)
            })),
        )
            .map(|(_, (_, ty, _, term))| Panic(false, ty, Box::new(term))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_print<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("__print"),
            cut((ws1_max1_nl, |input| parse_term_primary(syntaxes, input))),
        )
            .map(|(_, (_, term))| Print(Box::new(term))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_readline<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (position, tag("__readline").map(|_| ReadLine))
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_pure<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("__pure"),
            cut((ws1_max1_nl, parse_type_primary, ws1_max1_nl, |input| {
                parse_term_primary(syntaxes, input)
            })),
        )
            .map(|(_, (_, ty, _, term))| IOPure(ty, Box::new(term))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_bind<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("__bind"),
            cut((
                ws1_max1_nl,
                parse_type_primary,
                ws1_max1_nl,
                parse_type_primary,
                ws1_max1_nl,
                |input| parse_term_primary(syntaxes, input),
                ws1_max1_nl,
                |input| parse_term_primary(syntaxes, input),
            )),
        )
            .map(|(_, (_, dom, _, cod, _, func, _, t))| IOBind {
                dom,
                cod,
                func: Box::new(func),
                t: Box::new(t),
            }),
    )
        .map(SpannedToken::new)
        .parse(input)
}

fn parse_trace<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    (
        position,
        (
            tag("trace"),
            cut((ws1_max1_nl, parse_usize, ws1_max1_nl, |input| {
                parse_term(syntaxes, input)
            })),
        )
            .map(|(_, (_, i, _, term))| Trace(i, Box::new(term))),
    )
        .map(SpannedToken::new)
        .parse(input)
}

pub fn parse_comment<'a, 'b, E: ParseError<Span<'a>>>(input: Span<'a>) -> IResult<Span<'a>, (), E> {
    (tag("//"), take_till(|c: char| c.is_newline()))
        .map(|(_, _)| ())
        .parse(input)
}

/// Primary (always consuming) parser for terms.
pub fn parse_term_primary<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    alt((
        |input| parse_paren(syntaxes, input),
        parse_unit,
        parse_var,
        parse_int,
        parse_bool,
        |input| parse_pair(syntaxes, input),
        |input| parse_list(syntaxes, input),
        |input| parse_inl_inr(syntaxes, input),
        parse_nil,
        parse_readline,
        parse_char,
        parse_string_as_list_char,
    ))
    .parse(input)
}

/// Parses a term without any syntax extensions.
///
/// None of the sub-parsers should call parse_term directly
pub fn parse_term_base<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SpannedToken<'a>, E> {
    alt((
        |input| parse_ite(syntaxes, input),
        |input| parse_fst_snd(syntaxes, input),
        |input| parse_cons(syntaxes, input),
        |input| parse_lcase(syntaxes, input),
        |input| parse_case(syntaxes, input),
        |input| parse_let(syntaxes, input),
        |input| parse_binary_ops(syntaxes, input),
        |input| parse_abs(syntaxes, input),
        |input| parse_tabs(syntaxes, input),
        |input| parse_fix(syntaxes, input),
        |input| parse_panic(syntaxes, input),
        |input| parse_trace(syntaxes, input),
        |input| parse_print(syntaxes, input),
        |input| parse_pure(syntaxes, input),
        |input| parse_bind(syntaxes, input),
    ))
    .parse(input)
}

pub fn parse_term<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, SurfaceSpannedToken<'a>, E> {
    let syntax_operators = precedence::<_, _, E, _, _, _, _, _, _, _, (), _, _>(
        |input| syntaxes.prefix(input),
        fail(),
        |input| syntaxes.infix(input),
        |input| parse_app(syntaxes, input),
        |op: Operation<_, _, _, SpannedToken<'a>>| -> Result<_, E> {
            use nom_language::precedence::Operation::*;
            match op {
                Prefix((pos, op_sym), rhs) => Ok::<_, E>(SpannedToken {
                    position: pos,
                    token: Token::Prefix {
                        op: op_sym,
                        rhs: Box::new(rhs),
                    },
                    _state: PhantomData,
                }),
                Binary(lhs, (pos, op_sym), rhs) => Ok::<_, E>(SpannedToken {
                    position: pos,
                    token: Token::Infix {
                        op: op_sym,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    _state: PhantomData,
                }),
                _ => panic!("BUG! invalid operator"),
            }
        },
    );

    delimited(
        many0(ws0(parse_comment)),
        alt((syntax_operators, |input| parse_term_base(syntaxes, input))),
        many0(ws0(parse_comment)),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Span;
    use crate::syntax::Syntaxes;

    #[test]
    fn parses_type_applications_inside_application_chains() {
        let syntaxes = Syntaxes::new();

        let input = Span::new_extra("id Integer 1 (List Integer) 2", "");
        let (rest, _parsed) = parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
            .expect("parse_term failed");
        assert_eq!(*rest.fragment(), "");
    }

    #[test]
    fn does_not_consume_trailing_whitespace() {
        let syntaxes = Syntaxes::new();

        let input = Span::new_extra("123\n", "");
        let (rest, _parsed) = parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
            .expect("parse_term failed");
        assert_eq!(*rest.fragment(), "\n");

        let input = Span::new_extra("123\n\n// a\nf : Int -> Int\nf = fun x : Int, a\n", "");
        let (rest, _parsed) = parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
            .expect("parse_term failed");
        assert_eq!(
            *rest.fragment(),
            "\n\n// a\nf : Int -> Int\nf = fun x : Int, a\n"
        );
    }
}
