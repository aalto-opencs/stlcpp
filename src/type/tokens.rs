use crate::parse::*;

#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum Token<'a> {
    Boolean,
    Integer,
    Character,
    Unit,

    Arrow(Box<SpannedToken<'a>>, Box<SpannedToken<'a>>),
    Prod(Box<SpannedToken<'a>>, Box<SpannedToken<'a>>),
    List(Box<SpannedToken<'a>>),
    Sum(Box<SpannedToken<'a>>, Box<SpannedToken<'a>>),
    TVar(String),
    Forall {
        var: String,
        body: Box<SpannedToken<'a>>,
    },
    Hole,
    IO(Box<SpannedToken<'a>>),
}

#[derive(Debug, Clone)]
pub struct SpannedToken<'a> {
    pub token: Token<'a>,
    pub position: Span<'a>,
}

impl<'a> SpannedToken<'a> {
    pub fn new((position, token): (Span<'a>, Token<'a>)) -> Self {
        Self { token, position }
    }
}
