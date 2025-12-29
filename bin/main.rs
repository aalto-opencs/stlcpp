use nom::Parser;
use nom::combinator::all_consuming;
use stlcpp::errors::Error;
use stlcpp::module::{Import, ModuleTree};
use stlcpp::parse::{Span, ws0};
use stlcpp::term::Term;
use stlcpp::term::parse::{Desugared, SpannedToken, parse_term};
use stlcpp::r#type::named_type::NamedType;

use std::process::exit;
use std::{
    env::args,
    path::{Path, PathBuf},
};

use rustyline::{DefaultEditor, error::ReadlineError};

fn history_path() -> Option<PathBuf> {
    let home = std::env::var_os("HOME")?;
    Some(
        PathBuf::from(home)
            .join(".local")
            .join("share")
            .join("stlcpp")
            .join("history"),
    )
}

fn ensure_parent_dir(path: &Path) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    Ok(())
}

fn load_history_quiet_missing(rl: &mut DefaultEditor, path: &Path) {
    match rl.load_history(path) {
        Ok(()) => {}
        Err(ReadlineError::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => {
            // Fresh start: no history file yet.
        }
        Err(_e) => {
            // eprintln!("Failed to load history: {e}");
        }
    }
}

fn prompt<T: AsRef<str>>(file: Option<T>) -> String {
    match file {
        None => "λ ".to_string(),
        Some(name) => format!("{} λ ", name.as_ref()),
    }
}

/// Returns true if the program should exit
fn process_special(
    line: &str,
    module_tree: &ModuleTree,
    rl: &mut DefaultEditor,
) -> Result<bool, Error> {
    match line {
        ":q" => return Ok(true),
        ":env" => {
            let env = module_tree.env()?;
            let width = env
                .iter()
                .map(|(name, _)| name)
                .map(String::len)
                .max()
                .unwrap_or_default()
                .min(20);
            for (name, (_, ty)) in env {
                println!("{name:>width$} :: {ty}");
            }
        }
        ":alias" => {
            let env = module_tree.type_alias_env()?;
            let width = env
                .iter()
                .map(|(name, _)| name)
                .map(String::len)
                .max()
                .unwrap_or_default()
                .min(20);
            for (name, ty) in env {
                println!("{name:>width$} = {ty}");
            }
        }
        ":syntax" => {
            let syntaxes = module_tree.syntaxes();
            for s in syntaxes.infix {
                println!("{s}")
            }
            for s in syntaxes.prefix {
                println!("{s}")
            }
        }
        ":clear" => rl.clear_history().unwrap(),
        ":help" => {
            println!(
                "Welcome to STLC++, a functional programming language with focus on teaching PL design.

Available commands:
    :q          Quit. (CTRL-C and CTRL-D should also work)
    :env        List available declarations and their types.
    :alias      List available type declarations (aliases).
    :syntax     List available syntax extensions.
    :clear      Clear history.
    :help       Print this help.
    :exec       Execute IO value
"
            )
        }
        _ => {}
    }
    Ok(false)
}

fn process(line: &'static str, module_tree: &ModuleTree) -> Result<(), Box<dyn std::error::Error>> {
    let syntaxes = module_tree.syntaxes();

    let (exec, line) = if let Some(l) = line.strip_prefix(":exec") {
        (true, l)
    } else {
        (false, line)
    };

    let (_, body) = all_consuming(ws0(|input| {
        parse_term::<nom_language::error::VerboseError<_>>(&syntaxes, input)
    }))
    .parse(Span::new_extra(line, line))
    .map_err(|e| e.to_string())?;

    let env = module_tree.env()?;
    let aliases = module_tree.type_alias_env()?;

    let body: SpannedToken<'static, Desugared> = body
        .desugar(&syntaxes)
        .map_err(|e| format!("custom syntax desugaring failed: {e}"))?;

    let ty = body.type_check(env.clone().into(), &aliases)?;
    let t: Term = body
        .token
        .clone()
        .to_term_ctx_with_aliases(vec![], &aliases)?;

    let env = env
        .into_iter()
        .map(|(name, (term, _))| (name, term))
        .collect();

    let ty_str = ty.to_string();
    if exec {
        let res = t.exec(&env)?;
        println!("");
        if let NamedType::IO(ty1) = ty
            && *ty1 != NamedType::Unit
        {
            println!("{res}");
        }
    } else {
        let res = t.multistep(&env);
        if ty_str.len() > 10 {
            println!("{1}\n  : {0}", ty_str, res);
        } else {
            println!("{1} : {0}", ty_str, res);
        }
    }
    Ok(())
}

fn start_repl(
    file: Option<&str>,
    module_tree: ModuleTree,
    no_history: bool,
) -> Result<bool, Box<dyn std::error::Error>> {
    let p = prompt(file);
    let mut rl = DefaultEditor::new().unwrap();

    let history = if no_history { None } else { history_path() };
    if let Some(ref hp) = history {
        if let Err(_e) = ensure_parent_dir(hp) {
            // eprintln!("Failed to create history directory: {e}");
        } else {
            load_history_quiet_missing(&mut rl, hp);
        }
    }

    let mut last_error = false;
    let last_error = loop {
        let readline = rl.readline(&p);
        match readline {
            Ok(line) if line.trim() != "" => {
                rl.add_history_entry(line.as_str()).unwrap();
                if line.starts_with(":") && !line.starts_with(":exec ") {
                    if process_special(&line, &module_tree, &mut rl)? {
                        break false; // :q should not return last error
                    } else {
                        continue;
                    }
                }

                if let Err(e) = process(line.leak(), &module_tree) {
                    eprintln!("{e}");
                    last_error = true;
                } else {
                    last_error = false;
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break last_error;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break last_error;
            }
            _ => {}
        }
    };

    if let Some(ref hp) = history {
        if let Err(_e) = ensure_parent_dir(hp) {
            // eprintln!("Failed to create history directory: {e}");
        } else if let Err(_e) = rl.save_history(hp) {
            // eprintln!("Failed to save history: {e}");
        }
    }

    Ok(last_error)
}

fn prelude() -> Result<ModuleTree, Error> {
    Import("std/prelude.stlc".into()).resolve(
        std::env::var("STLCPP_PATH").unwrap_or_default(),
        &Default::default(),
    )
}

fn main_res() -> Result<bool, Box<dyn std::error::Error>> {
    let mut args = args();
    let _exe = args.next();

    let mut no_history = false;
    let mut exec = false;
    let mut file: Option<String> = None;

    for arg in args {
        // TODO use structopt and clap
        match arg.as_str() {
            "--no-history" => no_history = true,
            "--exec" => exec = true,
            _ if arg.starts_with('-') => {
                panic!("unknown flag '{arg}'")
            }
            _ => {
                // First non-flag argument is treated as the module path
                file = Some(arg);
                break;
            }
        }
    }

    let p = prelude()?;
    p.type_check()?;

    Ok(match file {
        Some(name) => {
            let i = Import(PathBuf::from(&name));
            let mt = i.resolve(PathBuf::default(), &p)?;

            mt.type_check()?;

            if exec {
                mt.exec_main()?;
                false
            } else {
                start_repl(Some(&name), mt, no_history)?
            }
        }
        None => {
            if exec {
                panic!("argument --exec requires a file argument");
            }

            start_repl(None, p, no_history)?
        }
    })
}

fn main() {
    match main_res() {
        Err(err) => {
            eprintln!("{err}");
            exit(1);
        }
        Ok(true) => exit(1),
        Ok(false) => {}
    }
}
