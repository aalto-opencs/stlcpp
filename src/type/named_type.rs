use std::collections::{HashMap, HashSet};

use crate::r#type::{
    TypeError,
    parse::{SpannedToken, Token},
};

/// A named type is a De Bruijn type where abstractions give names to variables.
#[derive(Debug, Clone)]
pub enum NamedType {
    Boolean,
    Integer,
    Character,
    Unit,
    Arrow(Box<NamedType>, Box<NamedType>),
    Prod(Box<NamedType>, Box<NamedType>),
    List(Box<NamedType>),
    Sum(Box<NamedType>, Box<NamedType>),

    /// De Bruijn indexed variable
    Var(usize),
    /// Abstraction with variable name
    Abs(String, Box<NamedType>),
    Hole,
    IO(Box<NamedType>),
}

use NamedType::*;

impl NamedType {
    /// When the main connective is an arrow, arrows and foralls need parens
    pub fn needs_parens_arrow(&self) -> bool {
        matches!(self, Arrow(..) | Abs { .. })
    }
    /// When the main connective is a product, arrows, foralls and sums need parens
    pub fn needs_parens_prod(&self) -> bool {
        matches!(self, Arrow(..) | Abs { .. } | Sum { .. })
    }
    pub fn needs_parens_sum(&self) -> bool {
        matches!(self, Arrow(..) | Abs { .. } | Sum { .. })
    }

    /// Calculates the maximum De Bruijn index of a free variable in the type.
    ///
    /// The result is normalized to the root scope. For example:
    /// - `Var(2)` at depth 0 returns `Some(2)`.
    /// - `Abs(Var(3))` (where depth is 1) returns `Some(2)` (because 3 - 1 = 2).
    /// - `Abs(Var(0))` (bound variable) returns `None`.
    pub fn max_free(&self) -> Option<usize> {
        self.max_free_at_depth(0)
    }

    pub fn is_closed_ctx(&self, ctx: &Vec<String>) -> bool {
        if let Some(f) = self.max_free()
            && f >= ctx.len()
        {
            false
        } else {
            true
        }
    }

    fn max_free_at_depth(&self, depth: usize) -> Option<usize> {
        match self {
            // A variable is free if its index is greater than or equal to the
            // current binding depth. We subtract depth to normalize it to the root.
            Self::Var(i) => {
                if *i >= depth {
                    Some(i - depth)
                } else {
                    None
                }
            }

            // When entering an abstraction, the depth increases.
            Self::Abs(_, body) => body.max_free_at_depth(depth + 1),

            // Recursive types: take the max of children.
            // Note: Option implements Ord, where None < Some(0) < Some(1).
            Self::Arrow(t1, t2) | Self::Prod(t1, t2) | Self::Sum(t1, t2) => {
                std::cmp::max(t1.max_free_at_depth(depth), t2.max_free_at_depth(depth))
            }

            Self::List(t1) | Self::IO(t1) => t1.max_free_at_depth(depth),

            // Base types and holes have no free variables.
            Self::Boolean | Self::Integer | Self::Character | Self::Unit | Self::Hole => None,
        }
    }

    pub fn contains_holes(&self) -> bool {
        match self {
            NamedType::Hole => true,
            NamedType::Arrow(ty1, ty2) | NamedType::Prod(ty1, ty2) | NamedType::Sum(ty1, ty2) => {
                ty1.contains_holes() || ty2.contains_holes()
            }
            NamedType::List(ty1) => ty1.contains_holes(),
            NamedType::Abs(_, body) => body.contains_holes(),
            _ => false,
        }
    }
}

// impl From<SpannedToken<'_>> for NamedType {
//     fn from(value: SpannedToken<'_>) -> Self {
//         // TODO: this should be a try from
//         value
//             .token
//             .to_named_type_ctx(vec![])
//             .expect("BUG: cannot name type")
//     }
// }

/// Helper to handle the SpannedToken wrapper.
fn unspan(spanned: SpannedToken<'_>) -> Token<'_> {
    spanned.token
}

impl Token<'_> {
    /// Convert a type parser [`Token`] into a [`NamedType`] using a type-variable context
    /// (outermost .. innermost) to resolve De Bruijn indices.
    ///
    /// This replaces the old free `convert(...)` function.
    // pub fn to_named_type_ctx(self, mut ctx: Vec<String>) -> Result<NamedType, TypeError<'static>> {
    //     Ok(match self {
    //         // Base Types
    //         Token::Boolean => NamedType::Boolean,
    //         Token::Integer => NamedType::Integer,
    //         Token::Character => NamedType::Character,
    //         Token::Unit => NamedType::Unit,
    //         Token::Hole => NamedType::Hole,

    //         // Variables
    //         Token::TVar(name) => {
    //             // We search for the variable name in ctx.
    //             // We search from the end (most recently bound) to the beginning.
    //             if let Some(index_from_start) = ctx.iter().rposition(|v| v == &name) {
    //                 // De Bruijn index 0 is the current (top) item.
    //                 // If the vector is [z, y, x], 'x' is at index 2.
    //                 // dist = 3 - 1 - 2 = 0.
    //                 let db_index = ctx.len() - 1 - index_from_start;
    //                 NamedType::Var(db_index)
    //             } else {
    //                 // Determine how to handle free variables (variables not found in ctx).
    //                 // For a standard compiler, this might be a panic or a specific error.
    //                 // TODO: could this be done on a spanned token so that we can get a position
    //                 return Err(TypeError::UndefinedVariable(name));
    //             }
    //         }

    //         // Abstractions (The core of De Bruijn conversion)
    //         Token::Forall { var, body } => {
    //             // 1. Push the variable name onto the stack
    //             ctx.push(var.clone());

    //             // 2. Recurse on the body with the new environment
    //             // Note: We need to extract the Token from SpannedToken here
    //             let body_token = unspan(*body);
    //             let converted_body = body_token.to_named_type_ctx(ctx.clone())?;

    //             NamedType::Abs(var, Box::new(converted_body))
    //         }

    //         // Recursive Structural Types
    //         Token::Arrow(param, body) => NamedType::Arrow(
    //             Box::new(unspan(*param).to_named_type_ctx(ctx.clone())?),
    //             Box::new(unspan(*body).to_named_type_ctx(ctx)?),
    //         ),
    //         Token::Prod(left, right) => NamedType::Prod(
    //             Box::new(unspan(*left).to_named_type_ctx(ctx.clone())?),
    //             Box::new(unspan(*right).to_named_type_ctx(ctx)?),
    //         ),
    //         Token::Sum(left, right) => NamedType::Sum(
    //             Box::new(unspan(*left).to_named_type_ctx(ctx.clone())?),
    //             Box::new(unspan(*right).to_named_type_ctx(ctx)?),
    //         ),
    //         Token::List(inner) => NamedType::List(Box::new(unspan(*inner).to_named_type_ctx(ctx)?)),
    //         Token::IO(inner) => NamedType::IO(Box::new(unspan(*inner).to_named_type_ctx(ctx)?)),
    //     })
    // }

    /// Convert a type parser [`Token`] into a [`NamedType`] using:
    /// - a type-variable context (`ctx`, outermost..innermost) for De Bruijn indices, and
    /// - a type-alias environment (`aliases`) for resolving module-level type declarations.
    ///
    /// In case there is shadowing, bound type variables take precedence over aliases.
    pub fn to_named_type_ctx_with_aliases(
        self,
        mut ctx: Vec<String>,
        aliases: &HashMap<String, NamedType>,
    ) -> Result<NamedType, TypeError<'static>> {
        Ok(match self {
            Token::Boolean => NamedType::Boolean,
            Token::Integer => NamedType::Integer,
            Token::Character => NamedType::Character,
            Token::Unit => NamedType::Unit,
            Token::Hole => NamedType::Hole,

            Token::TVar(name) => {
                if let Some(index_from_start) = ctx.iter().rposition(|v| v == &name) {
                    let db_index = ctx.len() - 1 - index_from_start;
                    NamedType::Var(db_index)
                } else if let Some(aliased) = aliases.get(&name) {
                    aliased.clone()
                } else {
                    return Err(TypeError::UndefinedVariable(name));
                }
            }

            Token::Forall { var, body } => {
                ctx.push(var.clone());
                let body_token = unspan(*body);
                let converted_body = body_token.to_named_type_ctx_with_aliases(ctx, aliases)?;
                NamedType::Abs(var, Box::new(converted_body))
            }

            Token::Arrow(param, body) => NamedType::Arrow(
                Box::new(unspan(*param).to_named_type_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(unspan(*body).to_named_type_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Prod(left, right) => NamedType::Prod(
                Box::new(unspan(*left).to_named_type_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(unspan(*right).to_named_type_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::Sum(left, right) => NamedType::Sum(
                Box::new(unspan(*left).to_named_type_ctx_with_aliases(ctx.clone(), aliases)?),
                Box::new(unspan(*right).to_named_type_ctx_with_aliases(ctx, aliases)?),
            ),
            Token::List(inner) => NamedType::List(Box::new(
                unspan(*inner).to_named_type_ctx_with_aliases(ctx, aliases)?,
            )),
            Token::IO(inner) => NamedType::IO(Box::new(
                unspan(*inner).to_named_type_ctx_with_aliases(ctx, aliases)?,
            )),
        })
    }
}

fn increment_indices(ctx: &mut HashMap<String, usize>) {
    ctx.values_mut().for_each(|v| *v += 1)
}

/// Convert a set of type variables to a map with unique indices.
fn context_to_indices(ctx: HashSet<String>) -> HashMap<String, usize> {
    ctx.into_iter().enumerate().map(|(i, s)| (s, i)).collect()
}

impl<'a> SpannedToken<'a> {
    // FIXME is this really needed? use to_named_type_ctx_with_aliases instead
    pub fn to_named(&'_ self, ctx: HashSet<String>) -> Result<NamedType, TypeError<'a>> {
        self.to_named_aux(context_to_indices(ctx))
    }

    pub fn to_named_aux(
        &'_ self,
        mut ctx: HashMap<String, usize>,
    ) -> Result<NamedType, TypeError<'a>> {
        use Token::*;
        Ok(match &self.token {
            Boolean => NamedType::Boolean,
            Integer => NamedType::Integer,
            Character => NamedType::Character,
            Unit => NamedType::Unit,
            TVar(v) => {
                if let Some(i) = ctx.get(v) {
                    NamedType::Var(*i)
                } else {
                    return Err(TypeError::FreeTypeVariable(v.clone()));
                }
            }
            Forall { var, body } => {
                increment_indices(&mut ctx);
                ctx.insert(var.clone(), 0);
                NamedType::Abs(var.clone(), Box::new(body.to_named_aux(ctx)?))
            }
            Arrow(ty1, ty2) => NamedType::Arrow(
                Box::new(ty1.to_named_aux(ctx.clone())?),
                Box::new(ty2.to_named_aux(ctx)?),
            ),
            Prod(ty1, ty2) => NamedType::Prod(
                Box::new(ty1.to_named_aux(ctx.clone())?),
                Box::new(ty2.to_named_aux(ctx)?),
            ),
            List(ty1) => NamedType::List(Box::new(ty1.to_named_aux(ctx.clone())?)),
            Sum(ty1, ty2) => NamedType::Sum(
                Box::new(ty1.to_named_aux(ctx.clone())?),
                Box::new(ty2.to_named_aux(ctx)?),
            ),
            Hole => NamedType::Hole,
            _ => todo!(),
        })
    }
}

impl std::fmt::Display for NamedType {
    /// Formats a named type.
    ///
    /// # Examples
    ///
    /// ```
    /// use stlcpp::r#type::named_type::NamedType;
    /// use stlcpp::r#type::util::*;
    ///
    /// let ty = abs("X", 0);
    /// assert_eq!(ty.to_string(), "forall X, X");
    ///
    /// let ty = abs("X", abs("Y", 0));
    /// assert_eq!(ty.to_string(), "forall X, forall Y, Y");
    ///
    /// let ty = abs("X", abs("Y", arrow(0, 1)));
    /// assert_eq!(ty.to_string(), "forall X, forall Y, Y -> X");
    ///
    /// let ty = abs("X", abs("X", arrow(0, 1)));
    /// assert_eq!(ty.to_string(), "forall X, forall X_1, X_1 -> X");
    ///
    /// let ty = abs("X", arrow(abs("X", arrow(0, 1)), 0));
    /// assert_eq!(ty.to_string(), "forall X, (forall X_1, X_1 -> X) -> X");
    ///
    /// let ty = abs("T", arrow(list(0), arrow(list(0), list(0))));
    /// assert_eq!(ty.to_string(), "forall T, [T] -> [T] -> [T]");
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_ctx(f, vec![])
    }
}

impl std::fmt::Display for SpannedToken<'_> {
    /// Note, the type must be complete, i.e. not have free type variables or unexpanded aliases.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let named_type: NamedType = self
            .token
            .clone()
            .to_named_type_ctx_with_aliases(vec![], &Default::default())
            .expect("BUG: cannot name type");
        named_type.fmt(f)
    }
}

impl PartialEq for NamedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean, Self::Boolean)
            | (Self::Integer, Self::Integer)
            | (Self::Character, Self::Character)
            | (Self::Unit, Self::Unit)
            | (Self::Hole, Self::Hole) => true,
            (Self::Var(i), Self::Var(j)) => i == j,
            (Self::Abs(_, ty1), Self::Abs(_, ty2))
            | (Self::List(ty1), Self::List(ty2))
            | (Self::IO(ty1), Self::IO(ty2)) => ty1 == ty2,
            (Self::Arrow(ty11, ty12), Self::Arrow(ty21, ty22))
            | (Self::Prod(ty11, ty12), Self::Prod(ty21, ty22))
            | (Self::Sum(ty11, ty12), Self::Sum(ty21, ty22)) => ty11 == ty21 && ty12 == ty22,

            _ => false,
        }
    }
}

impl Eq for NamedType {}
