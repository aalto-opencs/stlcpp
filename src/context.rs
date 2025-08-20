use std::collections::{HashMap, HashSet};

use crate::r#type::named_type::NamedType;

/// A variable declaration. Declarations are the building blocks for contexts.
///
/// A declaration is either a term variable declaration [`Declaration::Var`] or a type variable declaration [`Declaration::TVar]`.
/// A term variable declaration is pushed to the context when type checking term abstractions.
/// Likewise, a type variable declaration is pushed to the context when type checking type abstractions.
///
/// # References
/// See Definition 3.4.3 of Type Theory and Formal Proof.
#[derive(Debug, Clone)]
pub enum Declaration {
    Var(String, NamedType),
    TVar(String),
}

/// A second order typed lambda calculus context. Consists of term and type variable [`Declaration`]s.
/// Maintains the order of declarations.
///
/// The domain of a [`Context`] is defined as the set of all variable names in it.
/// As there is never a need for this set of all term and type variables, you can get one or the other with
/// - [`Context::domain`], which provides all term variables
/// - [`Context::domain_type`], which provides all type variables
///
/// # Inserting types
///
/// Given a fresh (not present in the context's domain) type variable name, one can insert that into the context with [`Context::insert_type`].
///
/// # Inserting terms
///
/// A type is properly formed (relative to a context) if all of its free variables are declared in the context.
/// Only properly formed types can be inserted into the context which ensures that all types within a context stay properly formed.
/// One can insert a term variable declaration into the context with [`Context::insert`].
/// Unlike definition 3.4.4, the term variable name does not need to be unique. This is because during type-checked evaluation, only complete terms are substituted in which guarantees that no variable is captured.
///
/// # References
/// See Definition 3.4.4 of Type Theory and Formal Proof.
#[derive(Debug, Clone, Default)]
pub struct Context(pub Vec<Declaration>);

impl Context {
    /// Create a new empty context.
    pub fn new() -> Self {
        Default::default()
    }

    /// Get the set of term variables within a context.
    pub fn domain(&self) -> HashSet<String> {
        self.0
            .iter()
            .filter_map(|e| match e {
                Declaration::Var(n, _) => Some(n.clone()),
                _ => None,
            })
            .collect()
    }

    /// Get the set of type variables within a context.
    pub fn tvars(&self) -> Vec<String> {
        self.0
            .iter()
            .filter_map(|e| match e {
                Declaration::TVar(n) => Some(n.clone()),
                _ => None,
            })
            .collect()
    }

    /// Insert a term variable declaration with a properly formed type into the context.
    ///
    /// The variable name does not need to be fresh.
    ///
    /// # Errors
    ///
    /// If the type was not properly formed relative to the context, a [`TypeError::NotProperlyFormed`] error is returned.
    pub fn insert(&mut self, var: String, ty: NamedType) {
        self.0.push(Declaration::Var(var, ty));
    }

    /// Insert a fresh type variable declaration into the context. Shadowing is impossible.
    pub fn insert_type(&mut self, var: String) {
        self.shift_tvars(1);
        self.0.push(Declaration::TVar(var));
    }

    pub fn shift_tvars(&mut self, amount: usize) {
        self.0.iter_mut().for_each(|d| match d {
            Declaration::Var(_, t) => {
                *t = t.to_owned().shift(amount, 0);
            }
            _ => {}
        })
    }

    pub fn get(&self, var: &String) -> Option<&NamedType> {
        self.0
            .iter()
            .rev()
            .filter_map(|v| match v {
                Declaration::Var(n, ty) if var == n => Some(ty),
                _ => None,
            })
            .next()
    }

    pub fn get_type(&self, var: &String) -> Option<()> {
        if self.0.iter().any(|v| match v {
            Declaration::TVar(n) if var == n => true,
            _ => false,
        }) {
            Some(())
        } else {
            None
        }
    }
}

impl From<HashMap<String, NamedType>> for Context {
    fn from(value: HashMap<String, NamedType>) -> Self {
        Context(
            value
                .into_iter()
                .map(|(name, ty)| Declaration::Var(name, ty))
                .collect(),
        )
    }
}
