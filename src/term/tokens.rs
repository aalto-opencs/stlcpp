use std::marker::PhantomData;

use crate::parse::*;
use crate::r#type::tokens::SpannedToken as TSpannedToken;

/// Build a string literal term as a list of characters (`[Char]`), encoded as nested `Cons(Char(_), ...)`
/// ending in `Nil(Char)`.
///
/// This mirrors the encoding used in `term::parse::parse_string_as_list_char`.
pub fn string_literal_as_list_char<'a, S>(
    s: impl AsRef<str>,
    pos: Span<'a>,
) -> SpannedToken<'a, S> {
    use crate::r#type::tokens::Token as TyToken;

    let mut acc = SpannedToken {
        position: pos,
        token: Token::Nil(TSpannedToken {
            token: TyToken::Character,
            position: pos,
        }),
        _state: PhantomData,
    };

    // Right fold so we preserve the original left-to-right order in the resulting list.
    for c in s.as_ref().chars().rev() {
        let head = SpannedToken {
            position: pos,
            token: Token::Char(c),
            _state: PhantomData,
        };
        acc = SpannedToken {
            position: pos,
            token: Token::Cons(Box::new(head), Box::new(acc)),
            _state: PhantomData,
        };
    }

    acc
}

#[derive(Debug, Clone, Copy)]
pub struct Surface;

#[derive(Debug, Clone, Copy)]
pub struct Desugared;

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

    /// Function composition
    Compose(Box<SpannedToken<'a, S>>, Box<SpannedToken<'a, S>>),

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
