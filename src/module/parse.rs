use std::{
    ffi::OsString,
    path::{Path, PathBuf},
};

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::*,
    combinator::{cut, eof, opt, recognize},
    error::ParseError,
    multi::{many0, separated_list1},
};
use nom_language::error::VerboseError;
use nom_locate::position;

use crate::{
    errors::Error,
    module::{Module, ModuleTree},
    parse::*,
    syntax::{parse::*, *},
    term::{
        parse::{parse_term, parse_variable_name},
        tokens::{SpannedToken as TermSpannedToken, Surface},
    },
    r#type::{
        parse::{parse_sort, parse_type, parse_type_variable_name},
        tokens::SpannedToken as TypeSpannedToken,
    },
};

use super::{Declaration, Import, TypeDeclaration};

fn skip_trivia<'a, E: ParseError<Span<'a>> + nom::error::ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(many0(alt((multispace1, parse_comment)))).parse(input)
}

/// ```stlc
/// import a.b
/// ```
///
/// Is converted to a `Import('a/b.stlc')`
pub fn parse_import<
    'a,
    E: ParseError<Span<'a>>
        + nom::error::ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, Import, E> {
    (
        many0(ws0(parse_comment)),
        tag("import"),
        cut((space1, separated_list1(char('.'), parse_variable_name))),
    )
        .map(|(_, _, (_, mut components))| {
            if let Some(filename) = components.pop() {
                let mut path = PathBuf::new();
                for component in components {
                    path.push(component);
                }
                path.push(format!("{filename}.stlc"));
                Import(path)
            } else {
                unreachable!("separated_list1 must create non empty vec")
            }
        })
        .parse(input)
}

/// Parse a declaration with syntax extensions
///
/// ```stlc
/// a : Integer
/// a = 5
/// ```
///
/// Is converted to a `Declaration("a", Integer, 5)`
pub fn parse_declaration<
    'a,
    'b,
    E: ParseError<Span<'a>>
        + nom::error::ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    syntaxes: &'b Syntaxes,
    input: Span<'a>,
) -> IResult<Span<'a>, (String, TypeSpannedToken<'a>, TermSpannedToken<'a, Surface>), E> {
    (
        position,
        parse_variable_name, // If variable name parser fails then it's fatal
        ws0(char(':')),
        parse_type,
        space0,
        opt((newline, parse_variable_name, ws0(char('=')), |input| {
            parse_term(syntaxes, input)
        })
            .map(|(_, var2, _, term)| (var2, term))),
    )
        .map(|(pos, var1, _, ty, _, def)| {
            if let Some((var2, term)) = def {
                if var1 == var2 {
                    (var1, ty, term)
                } else {
                    todo!()
                }
            } else {
                // Build the default panic message *before* moving `var1` into the declaration tuple.
                let msg = format!("declaration {var1} is unimplemented");

                (
                    var1,
                    ty.clone(),
                    TermSpannedToken {
                        token: crate::term::tokens::Token::Panic(
                            false,
                            ty,
                            Box::new(crate::term::tokens::string_literal_as_list_char(msg, pos)),
                        ),
                        position: pos,
                        _state: std::marker::PhantomData,
                    },
                )
            }
        })
        .parse(input)
}

/// Parse a type alias declaration with a required explicit sort signature.
///
/// ```stlc
/// State : Type
/// State = (Int, Bool)
/// ```
///
/// Is converted to a `TypeDeclaration("State", (Int, Bool))`
pub fn parse_type_declaration<
    'a,
    E: ParseError<Span<'a>>
        + nom::error::ContextError<Span<'a>>
        + nom::error::FromExternalError<Span<'a>, std::num::ParseIntError>
        + nom::error::FromExternalError<Span<'a>, num_bigint::ParseBigIntError>
        + nom::error::FromExternalError<Span<'a>, E>
        + 'a,
>(
    input: Span<'a>,
) -> IResult<Span<'a>, (String, TypeSpannedToken<'a>), E> {
    cut((
        parse_type_variable_name,
        ws0(char(':')),
        parse_sort,
        space0,
        newline,
        parse_type_variable_name,
        ws0(char('=')),
        parse_type,
    ))
    .map(|(name1, _, _, _, _, name2, _, ty)| {
        if name1 == name2 {
            (name1, ty)
        } else {
            todo!("handle error in nom")
        }
    })
    .parse(input)
}

/// A non-nom parser for parsing modules. Mutually recursive with Import::realize.
pub fn parse_module(
    basepath: impl AsRef<Path>,
    name: OsString,
    prelude: &ModuleTree,
    code: &'static str,
) -> Result<ModuleTree, Error> {
    let mut imports = vec![];
    let mut type_declarations = vec![];
    let mut declarations = vec![];
    let mut syntaxes = Syntaxes::new();
    // combined syntaxes accumulates local and foreign syntaxes to be used during parsing
    let mut combined_syntaxes = prelude.syntaxes();
    // The prelude is always a submodule. TODO add a file attribute to disable prelude
    let mut submodules = vec![prelude.clone()];

    enum Block<'a> {
        Import(Import),
        TypeDeclaration((String, TypeSpannedToken<'a>)),
        Declaration((String, TypeSpannedToken<'a>, TermSpannedToken<'a, Surface>)),
        Infix(Infix),
        Prefix(Prefix),
    }

    let mut input = Span::new_extra(code, code);
    loop {
        // Tolerate whitespace and comments:
        // consume trivia before we decide whether we're at EOF.
        let (rest, _) = skip_trivia(input)?;
        input = rest;

        if let Ok(_) = eof::<_, VerboseError<_>>.parse(input) {
            break;
        }

        let (rest, res) = alt((
            parse_import.map(Block::Import),
            (|input| parse_infix(&combined_syntaxes, input)).map(Block::Infix),
            (|input| parse_prefix(&combined_syntaxes, input)).map(Block::Prefix),
            (|input| parse_declaration(&combined_syntaxes, input)).map(Block::Declaration),
            parse_type_declaration.map(Block::TypeDeclaration),
        ))
        .parse(input)?; // TODO Instead of breaking here, we could collect parsing errors, go to next declaration and return partial result
        input = rest;

        match res {
            Block::Import(i) => {
                let mt = i.resolve(&basepath, prelude)?;
                combined_syntaxes.append(&mut mt.syntaxes());
                submodules.push(mt);
                imports.push(i)
            }
            Block::TypeDeclaration((name, ty)) => {
                type_declarations.push(TypeDeclaration { name, ty });
            }
            Block::Declaration((name, ty, term)) => {
                let desugared = term.desugar(&combined_syntaxes)?;
                declarations.push(Declaration {
                    name,
                    ty,
                    term: desugared,
                });
            }
            Block::Infix(infix) => {
                syntaxes.infix.push(infix.clone());
                combined_syntaxes.infix.push(infix);
            }
            Block::Prefix(prefix) => {
                syntaxes.prefix.push(prefix.clone());
                combined_syntaxes.prefix.push(prefix);
            }
        }
    }

    Ok(ModuleTree(
        Module {
            name,
            code,
            imports,
            type_declarations,
            declarations,
            syntaxes,
        },
        submodules,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trivia() {
        let input = Span::new_extra("abc", "");
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra("\nabc", "");
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra("// comment", "");
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "");

        let input = Span::new_extra("// comment\nabc", "");
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra(
            "
// comment

abc",
            "",
        );
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");

        let input = Span::new_extra(
            "
// comment
abc",
            "",
        );
        let (rest, _parsed) = skip_trivia::<nom_language::error::VerboseError<_>>(input).unwrap();
        assert_eq!(*rest.fragment(), "abc");
    }
}
