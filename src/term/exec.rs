use std::io::{self, Write};
use std::{collections::HashMap, io::stdout};

use crate::term::Term;
use crate::term::display::to_string_raw;
use crate::r#type::named_type::NamedType;

impl Term {
    pub fn exec(mut self, env: &HashMap<String, super::Term>) -> io::Result<Term> {
        use Term::*;
        self = self.multistep(env);
        let mut out = stdout().lock();
        match self {
            Print(t) => {
                write!(&mut out, "{}", to_string_raw(&t))?;
                out.flush()?;
                Ok(Trivial)
            }
            ReadLine => {
                let mut s = String::new();
                std::io::stdin().read_line(&mut s)?;
                s.clone()
                    .strip_suffix("\n")
                    .or(Some(s.as_str()))
                    .map(|s| {
                        s.chars().rfold(Nil(NamedType::Character), |acc, c| {
                            Self::Cons(Box::new(Self::Char(c)), Box::new(acc))
                        })
                    })
                    .ok_or_else(|| unreachable!())
            }
            IOPure(_ty, term) => Ok(*term),
            IOBind { func, t, .. } => App(func, Box::new(t.exec(env)?)).exec(env),

            _ => unreachable!("t should be a value of type IO something"),
        }
    }
}
