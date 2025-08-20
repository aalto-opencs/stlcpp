use std::fmt;

use nom::{IResult, error::ParseError};
use nom_language::precedence::{Assoc, Binary, Unary};
use nom_locate::position;

use crate::parse::{Span, alt_iter_longest};
use crate::term::parse::SpannedToken as TermSpannedToken;

pub mod parse;

/// An operator
#[derive(Debug, Clone, PartialEq)]
pub struct Operator(pub String);

// TODO get rid of 'static

#[derive(Clone)]
pub struct Infix {
    pub prec: usize,
    pub assoc: Assoc,
    pub lhs: String,
    pub op: Operator,
    pub rhs: String,
    pub body: TermSpannedToken<'static>,
}

impl fmt::Debug for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let assoc = match self.assoc {
            Assoc::Left => "Left",
            Assoc::Right => "Right",
        };

        f.debug_struct("Infix")
            .field("prec", &self.prec)
            .field("assoc", &assoc)
            .field("lhs", &self.lhs)
            .field("op", &self.op)
            .field("rhs", &self.rhs)
            .field("body", &"<term template>")
            .finish()
    }
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let assoc = match self.assoc {
            Assoc::Left => "l",
            Assoc::Right => "r",
        };
        let prec = &self.prec;
        let lhs = &self.lhs;
        let op = &self.op.0;
        let rhs = &self.rhs;
        // NOTE: Avoid converting the desugaring template into a core `Term` here.
        // That conversion can require additional context (e.g. module-level type aliases),
        // and Display/Debug for syntax declarations should not depend on module environments.
        write!(f, "infix{assoc}:{prec} {lhs} {op} {rhs} = <term template>")
    }
}

#[derive(Clone)]
pub struct Prefix {
    pub prec: usize,
    pub op: Operator,
    pub rhs: String,
    pub body: TermSpannedToken<'static>,
}

impl fmt::Debug for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Prefix")
            .field("prec", &self.prec)
            .field("op", &self.op)
            .field("rhs", &self.rhs)
            .field("body", &"<term template>")
            .finish()
    }
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prec = &self.prec;
        let op = &self.op.0;
        let rhs = &self.rhs;
        write!(f, "prefix:{prec} {op} {rhs} = <term template>")
    }
}

#[derive(Clone, Debug, Default)]
/// A syntax extension is like a macro that transforms or "desugars" a custom syntax into a core term.
///
/// For example, declaring syntax as follows
///
/// ```stlc
/// infixr x :: xs = cons x xs
/// ```
///
/// Will enable augmenting the parser with a rule for `x :: xs`.
///
/// The above syntax extension consists of a pattern [Identifier(x), Whitespace, Symbol(:), Symbol(:), Whitespace, Identifier(xs)].
pub struct Syntaxes {
    pub prefix: Vec<Prefix>,
    // pub postfix: Vec<Syntax>,
    pub infix: Vec<Infix>,
}
// TODO Collect syntaxes into one vector to represent the order in which they are declared

impl Syntaxes {
    pub fn new() -> Self {
        Default::default()
    }
    // fn prefix(&self) -> Parser {
    //     todo!()
    // }
    // fn postfix(&self) -> Parser {
    //     todo!()
    // }

    /// Parses a prefix operator occurrence and returns operator metadata for building
    /// a surface `Token::Prefix` node.
    ///
    /// The actual desugaring (`Prefix::body`) is intentionally *not* applied during parsing.
    pub fn prefix<'a, E>(
        &'_ self,
        input: Span<'a>,
    ) -> IResult<Span<'a>, Unary<(Span<'a>, &'a str), usize>, E>
    where
        E: ParseError<Span<'a>>,
    {
        alt_iter_longest(
            input,
            self.prefix
                .iter()
                .map(|prefix| |input| prefix.parse_op(input)),
        )
    }

    /// Parses an infix operator occurrence and returns operator metadata for building
    /// a surface `Token::Infix` node.
    ///
    /// The actual desugaring (`Infix::body`) is intentionally *not* applied during parsing.
    pub fn infix<'a, E>(
        &'_ self,
        input: Span<'a>,
    ) -> IResult<Span<'a>, Binary<(Span<'a>, &'a str), usize>, E>
    where
        E: ParseError<Span<'a>>,
    {
        alt_iter_longest(
            input,
            self.infix.iter().map(|infix| |input| infix.parse_op(input)),
        )
    }

    /// Lookup helper for desugaring: find the first infix syntax rule for the given operator symbol.
    pub fn find_prefix_rule(&self, op: &str) -> Option<&Prefix> {
        self.prefix.iter().find(|prefix| prefix.op.0 == op)
    }

    /// Lookup helper for desugaring: find the first infix syntax rule for the given operator symbol.
    pub fn find_infix_rule(&self, op: &str) -> Option<&Infix> {
        self.infix.iter().find(|infix| infix.op.0 == op)
    }

    pub fn append(&mut self, other: &mut Self) {
        self.prefix.append(&mut other.prefix);
        // self.postfix.append(&mut other.postfix);
        self.infix.append(&mut other.infix);
    }
}
