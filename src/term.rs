use std::collections::HashMap;

use crate::{
    term::tokens::{Desugared, Token},
    r#type::TypeError,
    r#type::named_type::NamedType,
};

pub mod desugar;
mod display;
pub mod exec;
pub mod parse;
pub mod step;
pub mod subst;
pub mod tokens;
pub mod util;

/// Represents a lambda calculus term.
///
/// Note that the PartialEq that is derived uses the PartialEq instance from Type which has some caveats.
/// See `[Type::eq]`
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A variable term, represented by a string.
    Var(String),
    /// An abstraction (lambda function) with a variable and a body term.
    Abs {
        var: String,
        ty: NamedType,
        body: Box<Term>,
    },
    /// An application of one term to another.
    App(Box<Term>, Box<Term>),

    /// A let expression assigning a variable `var` to a value `val_t` in `body`.
    /// It is effectively just a subtitution.
    Let {
        var: String,
        val_t: Box<Term>,
        body: Box<Term>,
    },

    /// A true boolean value
    True,
    /// A false boolean value
    False,
    /// If-then-else
    Ite {
        cond: Box<Term>,
        if_true: Box<Term>,
        if_false: Box<Term>,
    },

    /// An integer value
    Int(num_bigint::BigInt),
    /// Addition of two terms
    Add(Box<Term>, Box<Term>),
    /// Subtraction of two terms
    Sub(Box<Term>, Box<Term>),
    /// Multiplication of two terms
    Mul(Box<Term>, Box<Term>),
    /// Division of two terms
    Div(Box<Term>, Box<Term>),

    /// Equality comparison
    Eq(Box<Term>, Box<Term>),
    /// Non-equality
    Ne(Box<Term>, Box<Term>),
    /// Less than
    Lt(Box<Term>, Box<Term>),
    /// Less than or equal
    Le(Box<Term>, Box<Term>),
    /// Greater than
    Gt(Box<Term>, Box<Term>),
    /// Greater than or equal
    Ge(Box<Term>, Box<Term>),

    /// A unicode character/code-point
    Char(char),

    /// The element of Unit
    Trivial,

    /// A pair consisting of terms
    Pair(Box<Term>, Box<Term>),
    /// The first term in the pair
    Fst(Box<Term>),
    /// The second term in the pair
    Snd(Box<Term>),

    /// An empty list of some item type
    Nil(NamedType),
    /// The recursive constructor for lists, holds the head and the tail in the following order: `Cons(head, tail)`.
    Cons(Box<Term>, Box<Term>),
    /// Case analysis for lists
    ///
    /// ```text
    /// lcase t of
    /// | nil => nil_t
    /// | cons cons_var tail_var => cons_t
    /// ```
    LCase {
        t: Box<Term>,
        nil_t: Box<Term>,
        head_var: String,
        tail_var: String,
        cons_t: Box<Term>,
    },

    /// Injection to the left with the type of the right
    Inl(Box<Term>, NamedType),
    /// Injection to the right with the type of the left
    Inr(Box<Term>, NamedType),
    /// Case analysis for sum types
    ///
    /// ```text
    /// lcase t of
    /// | inl inl_var => inl_t
    /// | inr inr_var => inr_t
    /// ```
    Case {
        t: Box<Term>,
        inl_var: String,
        inl_t: Box<Term>,
        inr_var: String,
        inr_t: Box<Term>,
    },

    /// Fixed point combinator.
    /// Calculates the fixed point of the inner function.
    Fix(Box<Term>),

    /// A type abstraction
    TAbs {
        var: String,
        body: Box<Term>,
    },
    /// Application of a lambda term to a type
    TApp(Box<Term>, NamedType),

    /// A panic term. Evaluating this prints an error and as a side-effect stops further evaluation
    Panic(bool, NamedType, Box<Term>),

    Trace(usize, Box<Term>), // --> Term (side-effect: "trace: {term}")

    Print(Box<Term>),
    ReadLine,
    /// pure : forall A, A -> IO A
    IOPure(NamedType, Box<Term>),
    /// bind : forall A, forall B, (A -> IO B) -> IO A -> IO B
    IOBind {
        dom: NamedType,
        cod: NamedType,
        func: Box<Term>,
        t: Box<Term>,
    },
}

// x = fun A : Type, fun x : A, x
// TODO support fun short-hand
// fun (A) (x : A) (y : A), x + y
// -> fun A, fun x : A, fun y : A, x + y

// STLC++ examples
// `fun A : Type, fun x : A Int, x`
// `fun A, fun x : A Int, x`
// has type Forall A, A -> A

use Term::*;

impl Term {
    /// Determines whether the term is a value.
    ///
    /// Lambda abstractions [`Term::Abs`], [`Term::True`], [`Term::False`], [`Term::Int`] and [`Term::Nil`] are considered values.
    /// Let expressions, if-then-else expressions, comparison operators, applications, variables and taking fst or snd are not values.
    /// Case analysis on lists or sums is not a value.
    ///
    /// Pairs and [`Term::Cons`] are values only of both subterms are values.
    /// [`Term::Inl`] and [`Term::Inr`] are values only of the subterm is a value.
    ///
    /// # Examples
    ///
    /// **Abstraction is a value:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// # use stlcpp::r#type::named_type::NamedType::Boolean;
    /// assert!(abs("x", Boolean, var("x")).is_value());
    /// ```
    ///
    /// **True and false are values:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert!(True.is_value());
    /// assert!(False.is_value());
    /// ```
    ///
    /// **Variable is not a value:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert!(!var("x").is_value());
    /// ```
    ///
    /// **Application is not a value:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert!(!app(id22(), id2()).is_value());
    /// ```
    ///
    /// **Let expression is not a value:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// let let_expr = letin("x", id2(), var("x"));
    /// assert!(!let_expr.is_value());
    /// ```
    ///
    /// **Pair is value if subterms are values:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert!(pair(id2(), id2()).is_value());
    /// assert!(!pair(app(id22(), id2()), id2()).is_value());
    /// assert!(!pair(id2(), app(id22(), id2())).is_value());
    /// ```
    ///
    /// **Cons is value if subterms are values:**
    /// ```rust
    /// # use stlcpp::term::util::*;
    /// assert!(cons(id2(), id2()).is_value());
    /// assert!(!cons(app(id22(), id2()), id2()).is_value());
    /// assert!(!cons(id2(), app(id22(), id2())).is_value());
    /// ```
    pub fn is_value(&self) -> bool {
        match self {
            Abs { .. }
            | TAbs { .. }
            | True
            | False
            | Int(_)
            | Nil(_)
            | Char(_)
            | Trivial
            | ReadLine => true,
            Pair(t1, t2) | Cons(t1, t2) => t1.is_value() && t2.is_value(),
            Inl(t, _) | Inr(t, _) => t.is_value(),
            Panic(true, _, _) => true,
            Print(t) => t.is_value(),
            IOPure(_, t) => t.is_value(),
            IOBind { func, t, .. } => func.is_value() && t.is_value(),
            Var(_) => false, // Not value because of env in step
            _ => false,
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            Panic(true, _, _) => true,
            _ => false,
        }
    }

    /// Whether the term contains [`Type::Hole`](holes)
    pub fn contains_holes(&self) -> bool {
        // TODO move to SpannedToken instead of Term
        match self {
            Var(_) => false,
            Abs { ty, body: term, .. } | Inr(term, ty) | Inl(term, ty) | TApp(term, ty) => {
                ty.contains_holes() || term.contains_holes()
            }

            App(term, term1)
            | Add(term, term1)
            | Sub(term, term1)
            | Mul(term, term1)
            | Eq(term, term1)
            | Ne(term, term1)
            | Lt(term, term1)
            | Le(term, term1)
            | Gt(term, term1)
            | Ge(term, term1)
            | Pair(term, term1)
            | Cons(term, term1)
            | Let {
                val_t: term,
                body: term1,
                ..
            } => term.contains_holes() || term1.contains_holes(),

            Ite {
                cond,
                if_true,
                if_false,
            } => cond.contains_holes() || if_true.contains_holes() || if_false.contains_holes(),

            Trace(_, term) | Fst(term) | Snd(term) | Fix(term) | TAbs { body: term, .. } => {
                term.contains_holes()
            }
            Nil(ty1) => ty1.contains_holes(),
            LCase {
                t,
                nil_t: t1,
                cons_t: t2,
                ..
            }
            | Case {
                t,
                inl_t: t1,
                inr_t: t2,
                ..
            } => t.contains_holes() || t1.contains_holes() || t2.contains_holes(),

            _ => false,
        }
    }
}

impl<'a> From<Box<Token<'a>>> for Token<'a> {
    fn from(value: Box<Token<'a>>) -> Self {
        *value
    }
}

impl Token<'_, Desugared> {
    // TODO take any iterator of &str
    pub fn to_term_ctx(self, ctx: Vec<String>) -> Result<Term, TypeError<'static>> {
        // Preserve existing behavior for callers that don’t have a module-level alias env.
        let aliases: HashMap<String, NamedType> = HashMap::new();
        self.to_term_ctx_with_aliases(ctx, &aliases)
    }

    pub fn to_term_ctx_with_aliases(
        self,
        mut ctx: Vec<String>,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<Term, TypeError<'static>> {
        Ok(match self {
            Token::Var(v) => Term::Var(v),
            Token::Abs { var, ty, body } => Term::Abs {
                var,
                ty: ty
                    .token
                    .clone()
                    .to_named_type_ctx_with_aliases(ctx.clone(), aliases)?,
                body: Box::new(body.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::App(term, term1) => Term::App(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Let { var, val_t, body } => Term::Let {
                var,
                val_t: Box::new(val_t.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                body: Box::new(body.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::True => Term::True,
            Token::False => Term::False,
            Token::Ite {
                cond,
                if_true,
                if_false,
            } => Term::Ite {
                cond: Box::new(cond.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                if_true: Box::new(
                    if_true
                        .token
                        .to_term_ctx_with_aliases(ctx.clone(), aliases)?,
                ),
                if_false: Box::new(if_false.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::Int(n) => Term::Int(n),
            Token::Add(term, term1) => Term::Add(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Sub(term, term1) => Term::Sub(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Mul(term, term1) => Term::Mul(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Div(term, term1) => Term::Div(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Eq(term, term1) => Term::Eq(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Ne(term, term1) => Term::Ne(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Lt(term, term1) => Term::Lt(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Le(term, term1) => Term::Le(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Gt(term, term1) => Term::Gt(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Ge(term, term1) => Term::Ge(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Char(c) => Term::Char(c),
            Token::Trivial => Term::Trivial,
            Token::Pair(term, term1) => Term::Pair(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Fst(term) => {
                Term::Fst(Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?))
            }
            Token::Snd(term) => {
                Term::Snd(Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?))
            }
            Token::Nil(ty) => Term::Nil(ty.token.to_named_type_ctx_with_aliases(ctx, aliases)?),
            Token::Cons(term, term1) => Term::Cons(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(term1.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::LCase {
                t,
                nil_t,
                head_var,
                tail_var,
                cons_t,
            } => Term::LCase {
                t: Box::new(t.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                nil_t: Box::new(nil_t.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                head_var,
                tail_var,
                cons_t: Box::new(cons_t.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::Inl(term, ty) => Term::Inl(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                ty.token.to_named_type_ctx_with_aliases(ctx, aliases)?,
            ),
            Token::Inr(term, ty) => Term::Inr(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                ty.token.to_named_type_ctx_with_aliases(ctx, aliases)?,
            ),
            Token::Case {
                t,
                inl_var,
                inl_t,
                inr_var,
                inr_t,
            } => Term::Case {
                t: Box::new(t.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                inl_var,
                inl_t: Box::new(inl_t.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                inr_var,
                inr_t: Box::new(inr_t.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::Fix(term) => {
                Term::Fix(Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?))
            }
            Token::TAbs { var, body } => {
                ctx.push(var.clone());
                Term::TAbs {
                    var,
                    body: Box::new(body.token.to_term_ctx_with_aliases(ctx, aliases)?),
                }
            }
            Token::TApp(term, ty) => Term::TApp(
                Box::new(term.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                ty.token.to_named_type_ctx_with_aliases(ctx, aliases)?,
            ),
            Token::Panic(n, ty, term) => Term::Panic(
                n,
                ty.token
                    .to_named_type_ctx_with_aliases(ctx.clone(), aliases)?,
                Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Trace(n, term) => Term::Trace(
                n,
                Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Print(term) => {
                Term::Print(Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?))
            }
            Token::ReadLine => Term::ReadLine,
            Token::IOPure(ty, term) => Term::IOPure(
                ty.token
                    .to_named_type_ctx_with_aliases(ctx.clone(), aliases)?,
                Box::new(term.token.to_term_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::IOBind { dom, cod, func, t } => Term::IOBind {
                dom: dom
                    .token
                    .to_named_type_ctx_with_aliases(ctx.clone(), aliases)?,
                cod: cod
                    .token
                    .to_named_type_ctx_with_aliases(ctx.clone(), aliases)?,
                func: Box::new(func.token.to_term_ctx_with_aliases(ctx.clone(), aliases)?),
                t: Box::new(t.token.to_term_ctx_with_aliases(ctx, aliases)?),
            },
            Token::Infix { .. } | Token::Prefix { .. } => {
                unreachable!("BUG: desugared token should not contain custom syntax")
            }
        })
    }
}
