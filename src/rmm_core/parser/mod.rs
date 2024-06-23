mod expression;

use expression::{EntryPoint, TokenIter};
use super::Token;

#[derive(Debug)]
pub(crate) struct Parser {
    tokens: Vec<Token>,
    entry_point: Option<EntryPoint>
}
impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, entry_point: None }
    }

    pub(crate) fn parse(&mut self) {
        let mut iter = TokenIter(self.tokens.iter().peekable());
        self.entry_point = Some(match EntryPoint::parse(&mut iter) {
            Some(et) => et,
            None => panic!("Erro ao tentar parsear o codigo!"),
        });
    }

    pub(crate) fn translate(&self) -> String {
        match &self.entry_point {
            Some(et) => et.translate(),
            None => panic!("O codigo nao possui ponto de entrada!"),
        }
    }
}
impl std::fmt::Display for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.entry_point {
            Some(et) => write!(f, "{et}"),
            None => write!(f, "O codigo nao possui ponto de entrada!"),
        }
    }
}