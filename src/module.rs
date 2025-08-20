//! # Modules
//!
//! Modules are collections of declarations.
//! A declaration consists of a name, a type and a term.
//! A declaration may refer to any previous declaration or to itself recursively.
//!
//! For example the following code represents a module:
//!
//! ```stlc
//! x : Integer
//! x = 5
//!
//! y : Integer
//! y = x
//! ```
//!
//! The main entrypoint to using a module is [`Module::to_term`].

use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use crate::context::{self, Context};
use crate::errors::Error;
use crate::syntax::Syntaxes;
use crate::term::Term::{self};
use crate::term::parse::{Desugared, SpannedToken};
use crate::r#type::named_type::NamedType;
use crate::r#type::parse::SpannedToken as TSpannedToken;
use parse::parse_module;

pub mod parse;

#[derive(Debug, Clone)]
pub struct Declaration<'a> {
    pub name: String,
    pub ty: TSpannedToken<'a>,
    pub term: SpannedToken<'a, Desugared>,
}

/// A type declaration (type alias) at module scope.
///
/// Example:
/// ```stlc
/// State = (Int, Bool)
/// ```
#[derive(Debug, Clone)]
pub struct TypeDeclaration<'a> {
    pub name: String,
    pub ty: TSpannedToken<'a>,
}

#[derive(Debug, Clone, PartialEq)]
/// An import statement.
///
/// ```stlc
/// import a.b
/// ```
///
/// imports the file `a/b.stlc`.
pub struct Import(pub PathBuf);

impl Import {
    pub fn file_name(&self) -> &OsStr {
        self.0.file_name().expect("import to have file name")
    }

    /// Resolves an import into a [`ModuleTree`] with an additional prelude.
    pub fn resolve<'a>(
        &'a self,
        basepath: impl AsRef<Path>,
        prelude: &ModuleTree,
    ) -> Result<ModuleTree, Error> {
        let import_path = basepath.as_ref().join(&self.0);
        let import_base = import_path.parent().expect("import to have a parent"); // TODO handle error

        let name = self.0.file_name().unwrap().to_owned();
        let code = if let Some(c) = crate::vfs::get_file(&import_path) {
            c
        } else {
            String::leak(read_to_string(&import_path)?)
        };

        parse_module(import_base, name, prelude, code)
    }
}

#[derive(Clone, Default)]
pub struct Module {
    pub name: OsString,
    pub code: &'static str,
    pub imports: Vec<Import>,
    pub type_declarations: Vec<TypeDeclaration<'static>>,
    pub declarations: Vec<Declaration<'static>>,
    pub syntaxes: Syntaxes,
}

#[derive(Clone, Default)]
pub struct ModuleTree(pub(crate) Module, pub(crate) Vec<ModuleTree>);

impl ModuleTree {
    /// Construct a module tree node.
    ///
    /// This is needed outside this crate module (e.g. from the binary) because `ModuleTree`
    /// has private tuple fields.
    pub fn new(module: Module, children: Vec<ModuleTree>) -> Self {
        Self(module, children)
    }

    /// Collect all syntaxes used by this module tree into a single `Syntaxes`.
    ///
    /// This is *transient* in the sense that it builds a fresh `Syntaxes` value on every call,
    /// by concatenating (in pre-order) the syntaxes of each node in the tree.
    pub fn syntaxes(&self) -> Syntaxes {
        let mut acc = Syntaxes::new();
        self.syntaxes_aux(&mut acc);
        acc
    }

    fn syntaxes_aux(&self, acc: &mut Syntaxes) {
        // Parent syntax extensions should be available after they're declared,
        // and imported syntaxes should be available too, so we aggregate everything.
        acc.append(&mut self.0.syntaxes.clone());

        for child in &self.1 {
            child.syntaxes_aux(acc);
        }
    }

    /// Aggregate module-level type declarations (type aliases) into a single environment.
    ///
    /// Semantics:
    /// - aliases from imported modules are available
    /// - within a module, aliases are available *after* they are declared
    /// - an alias may reference earlier aliases (local or imported)
    /// - aliases cannot be mutually recursive because only earlier aliases exist during resolution
    pub fn type_alias_env(&self) -> Result<HashMap<String, NamedType>, Error> {
        let mut aliases: HashMap<String, NamedType> = HashMap::new();

        // Imports first (pre-order), then local decls.
        for m in &self.1 {
            aliases.extend(m.type_alias_env()?);
        }

        for td in &self.0.type_declarations {
            // Allow referencing earlier aliases (including those from imports), but not later ones.
            // We resolve incrementally against the `aliases` map being built.
            let name = td.name.clone();

            // Reject duplicates early to avoid ambiguity (includes imported aliases).
            if aliases.contains_key(&name) {
                return Err(Error::ImportError(
                    PathBuf::from(self.0.name.to_string_lossy().to_string()),
                    format!(
                        "duplicate type declaration '{name}': \
type declarations must be unique (including across imports)"
                    ),
                ));
            }

            // Convert the parsed type into a NamedType, resolving TVars via `aliases`.
            // Bound type variables still shadow aliases (handled in to_named_type_ctx_with_aliases).
            let ty = match td
                .ty
                .token
                .clone()
                .to_named_type_ctx_with_aliases(vec![], &aliases)
            {
                Ok(ty) => ty,
                Err(e) => {
                    // Make the common failure mode (referencing a later alias) more helpful.
                    // Example:
                    //   B : Type
                    //   B = A      // A declared later -> error
                    //   A : Type
                    //   A = ...
                    //
                    // Under the hood this shows up as "undefined variable".
                    return Err(Error::ImportError(
                        PathBuf::from(self.0.name.to_string_lossy().to_string()),
                        format!(
                            "invalid type declaration '{name}': {e}\n\
Hint: type aliases must be declared before they are used, and only earlier aliases (including imports) are in scope."
                        ),
                    ));
                }
            };

            if ty.contains_holes() {
                return Err(Error::ImportError(
                    PathBuf::from(self.0.name.to_string_lossy().to_string()),
                    format!(
                        "invalid type declaration '{name}': type aliases cannot contain holes (`_`)"
                    ),
                ));
            }

            aliases.insert(name, ty);
        }

        Ok(aliases)
    }

    pub fn env(&self) -> Result<Vec<(String, (Term, NamedType))>, Error> {
        let aliases = self.type_alias_env()?;

        let mut env = vec![];
        for m in &self.1 {
            env.extend(m.env()?);
        }
        for d in &self.0.declarations {
            let fix_term = d.term_fix();
            let term = fix_term.token.clone().to_term_ctx_with_aliases(
                env.iter().map(|(k, _v)| k).cloned().collect(),
                &aliases,
            )?;
            let ty = fix_term.type_check(env.clone().into(), &aliases)?;
            env.push((d.name.clone(), (term, ty)))
        }

        Ok(env)
    }
}

impl From<Vec<(String, (Term, NamedType))>> for Context {
    fn from(value: Vec<(String, (Term, NamedType))>) -> Self {
        Context(
            value
                .into_iter()
                .map(|(name, (_, ty))| context::Declaration::Var(name, ty))
                .collect(),
        )
    }
}
