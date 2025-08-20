pub mod context;
pub mod errors;
pub mod module;
pub mod parse;
pub mod syntax;
pub mod term;
pub mod r#type;
pub mod util;
pub mod vfs;
pub mod wasm;

pub static RESERVED_KEYWORDS: [&str; 46] = [
    "fun",
    "forall",
    "let",
    "in",
    "if",
    "then",
    "else",
    "true",
    "false",
    "fst",
    "snd",
    "inl",
    "inr",
    "case",
    "lcase",
    "of",
    "nil",
    "cons",
    "fix",
    "infixl",
    "infixr",
    "prefix",
    "postfix",
    "panic",
    "trace",
    // types
    "Integer",
    "Boolean",
    "Character",
    "Unit",
    "List",
    "IO",
    // NOTE: "Type" is a sort keyword, but we don't have sort variable so it doesn't need to be a reserved keyword.
    "_",
    // operators
    "__add",
    "__mul",
    "__sub",
    "__div",
    "__eq",
    "__ne",
    "__gt",
    "__lt",
    "__ge",
    "__le",
    "__print",
    "__readline",
    "__pure",
    "__bind",
];
