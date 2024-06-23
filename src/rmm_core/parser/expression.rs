use crate::rmm_core::Token;
use std::iter::Peekable;

#[derive(Clone)]
pub(super) struct TokenIter<'a, I: Iterator<Item = &'a Token>>(pub(super) Peekable<I>);

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// -------------------------------------- Declaracoes -------------------------------------
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#[derive(Debug, Clone)]
pub(super) enum ExpressaoPrimaria {
    Id(Token),
    Const(Token),
    Nested(Box<ExpressaoAritmeticaAditiva>),
}
impl ExpressaoPrimaria {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoPrimaria::Id(tk)
            | ExpressaoPrimaria::Const(tk) => tk.to_cpp(),

            ExpressaoPrimaria::Nested(nt) => std::format!("({})", nt.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoPrimaria {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Id(val) 
            | Self::Const(val) => write!(f, "{val}"),
            ExpressaoPrimaria::Nested(val) => write!(f, "{val}"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoAtribuicao {
    Char {
        tipo: Token,
        id: Token,
        value: Token,
    },
    Number {
        tipo: Token,
        id: Token,
        value: ExpressaoAritmeticaAditiva
    },
    Other {
        tipo: Token,
        id: Token,
        value: ExpressaoPrimaria,
    },
    Reasign {
        id: Token,
        value: ExpressaoPrimaria,
    }
}
impl ExpressaoAtribuicao {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoAtribuicao::Char { tipo, id, value } => std::format!("{} {} = {}", tipo.to_cpp(), id.to_cpp(), value.to_cpp()),
            ExpressaoAtribuicao::Number { tipo, id, value } => std::format!("{} {} = {}", tipo.to_cpp(), id.to_cpp(), value.translate()),
            ExpressaoAtribuicao::Other { tipo, id, value } => std::format!("{} {} = {}", tipo.to_cpp(), id.to_cpp(), value.translate()),
            ExpressaoAtribuicao::Reasign { id, value } => std::format!("{} = {}", id.to_cpp(), value.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoAtribuicao {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressaoAtribuicao::Number { tipo, id, value } => write!(f, "{tipo} {id} -> {value}"),
            ExpressaoAtribuicao::Other { tipo, id, value } => write!(f, "{tipo} {id} -> {value}"),
            ExpressaoAtribuicao::Char { tipo, id, value } => write!(f, "{tipo} {id} -> {value}"),
            ExpressaoAtribuicao::Reasign { id, value } => write!(f, "{id} -> {value}"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoRelacionalOu {
    RelacionalAnd(ExpressaoRelacionalAnd),
    Struct {
        left: Box<Self>,
        op: Token,
        right: Box<ExpressaoRelacionalAnd>,
    }
}
impl ExpressaoRelacionalOu {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoRelacionalOu::RelacionalAnd(expr) => expr.translate(),
            ExpressaoRelacionalOu::Struct { left, op, right } => std::format!("{} {} {}", left.translate(), op.to_cpp(), right.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoRelacionalOu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RelacionalAnd(val) => write!(f, "{val}"),
            Self::Struct { left, op, right } => {
                write!(f, "({left}, {op}, {right})")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoRelacionalAnd {
    Relacional(ExpressaoRelacional),
    Struct {
        left: Box<Self>,
        op: Token,
        right: Box<ExpressaoRelacional>,
    }
}
impl ExpressaoRelacionalAnd {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoRelacionalAnd::Relacional(expr) => expr.translate(),
            ExpressaoRelacionalAnd::Struct { left, op, right } => std::format!("{} {} {}", left.translate(), op.to_cpp(), right.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoRelacionalAnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Relacional(val) => write!(f, "{val}"),
            Self::Struct { left, op, right } => {
                write!(f, "({left}, {op}, {right})")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoRelacional {
    Primaria(ExpressaoPrimaria),
    Struct {
        left: Box<ExpressaoPrimaria>,
        op: Token,
        right: Box<ExpressaoPrimaria>,
    },
    Nested(Box<ExpressaoRelacionalOu>),
}
impl ExpressaoRelacional {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoRelacional::Primaria(expr) => expr.translate(),
            ExpressaoRelacional::Struct { left, op, right } => std::format!("{} {} {}", left.translate(), op.to_cpp(), right.translate()),
            ExpressaoRelacional::Nested(expr) => std::format!("({})", expr.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoRelacional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primaria(val) => write!(f, "{val}"),
            Self::Struct { left, op, right } => {
                write!(f, "({left}, {op}, {right})")
            },
            Self::Nested(val) => write!(f, "[{val}]")
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoAritmeticaAditiva {
    Multiplicativa(ExpressaoAritmeticaMultiplicativa),
    Operacao {
        left: Box<Self>,
        op: Token,
        right: Box<ExpressaoAritmeticaMultiplicativa>,
    }
}
impl ExpressaoAritmeticaAditiva {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoAritmeticaAditiva::Multiplicativa(mt) => mt.translate(),
            ExpressaoAritmeticaAditiva::Operacao { left, op, right } => std::format!("{} {} {}", left.translate(), op.to_cpp(), right.translate()),
        }
    }
}
impl std::fmt::Display for ExpressaoAritmeticaAditiva {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressaoAritmeticaAditiva::Multiplicativa(val) => write!(f, "{val}"),
            ExpressaoAritmeticaAditiva::Operacao { left, op, right } => write!(f, "({left}, {op}, {right})"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ExpressaoAritmeticaMultiplicativa {
    Primaria(ExpressaoPrimaria),
    Operacao {
        left: Box<Self>,
        op: Token,
        right: Box<ExpressaoPrimaria>,
    },
}
impl ExpressaoAritmeticaMultiplicativa {
    pub(super) fn translate(&self) -> String {
        match self {
            ExpressaoAritmeticaMultiplicativa::Primaria(expr) => expr.translate(),
            ExpressaoAritmeticaMultiplicativa::Operacao { left, op, right } => std::format!("{} {} {}", left.translate(), op.to_cpp(), right.translate())
        }
    }
}
impl std::fmt::Display for ExpressaoAritmeticaMultiplicativa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressaoAritmeticaMultiplicativa::Primaria(val) => write!(f, "{val}"),
            ExpressaoAritmeticaMultiplicativa::Operacao { left, op, right } => write!(f, "{{{left}, {op}, {right}}}"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum Expressao {
    Relacional(ExpressaoRelacionalOu),
    Aritmetica(ExpressaoAritmeticaAditiva),
    Atribuicao(ExpressaoAtribuicao),
}
impl Expressao {
    pub(super) fn translate(&self) -> String {
        match self {
            Expressao::Relacional(expr) => std::format!("{};", expr.translate()),
            Expressao::Aritmetica(expr) => std::format!("{};", expr.translate()),
            Expressao::Atribuicao(expr) => std::format!("{};", expr.translate()),
        }
    }
}
impl std::fmt::Display for Expressao {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expressao::Relacional(val) => write!(f, "-{val}-"),
            Expressao::Aritmetica(val) => write!(f, "-{val}-"),
            Expressao::Atribuicao(val) => write!(f, "-{val}-"),
        }
    }
}

impl ExpressaoPrimaria {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>
    {
        if let Some(token) = tokens.0.next() { 
            match token {
            Token::CONSTANTE_CHAR(_)
            | Token::CONSTANTE_INT(_)
            | Token::CONSTANTE_FLOAT(_) => Some(Self::Const(token.clone())),

            Token::IDENTIFICADOR(_) => Some(Self::Id(token.clone())),
            
            Token::ABRE_PARENTESE => {
                let expr = ExpressaoAritmeticaAditiva::parse(tokens)?;
                if let Some(Token::FECHA_PARENTESE) = tokens.0.next() {
                    Some(Self::Nested(Box::new(expr)))
                }
                else {
                    None
                }
            },

            _ => None,
        }}
        else {
            None
        }
    }
}

impl ExpressaoAtribuicao {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>
    {
        if let Some(tipo) = tokens.0.next() {
            if *tipo == Token::CHAR
            || *tipo == Token::INT32
            || *tipo == Token::FLOAT32 {
                if let (
                    Some(Token::IDENTIFICADOR(id)),
                    Some(Token::ATRIBUICAO)
                ) = (tokens.0.next(), tokens.0.next()) 
                {
                    if *tipo == Token::CHAR {
                        let token = tokens.0.next()?;
                        match token {
                            Token::CONSTANTE_CHAR(_) | Token::IDENTIFICADOR(_) => {
                                Some(Self::Char { 
                                    tipo: Token::CHAR, 
                                    id: Token::IDENTIFICADOR(id.clone()), 
                                    value: token.clone(),
                                })        
                            }
                            _ => None                        
                        }
                    }
                    else {
                        let mut iter_clone = tokens.clone();
                        if let Some(arit) = ExpressaoAritmeticaAditiva::parse(&mut iter_clone) {
                            *tokens = iter_clone;
                            Some(Self::Number { 
                                tipo: tipo.clone(),
                                id: Token::IDENTIFICADOR(id.clone()), 
                                value: arit, 
                            })
                        }
                        else {
                            Some(Self::Other { 
                                tipo: tipo.clone(), 
                                id: Token::IDENTIFICADOR(id.clone()), 
                                value: ExpressaoPrimaria::parse(tokens)?,
                            })                            
                        }
                    }
                }
                else {
                    None
                }
            }
            else {
                match tipo {
                    Token::IDENTIFICADOR(_) => {
                        if let Some(Token::ATRIBUICAO) = tokens.0.next() {
                            let value = ExpressaoPrimaria::parse(tokens)?;
                            
                            return Some(Self::Reasign { id: tipo.clone(), value })
                        }
                    },
                    
                    _ => return None,
                }
                
                return None
            }
        }
        else {
            None
        }
    }
}

impl ExpressaoRelacionalOu {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self> 
    where 
        I: Clone + Iterator<Item = &'a Token>
    {
        let left = ExpressaoRelacionalAnd::parse(tokens)?;
        let mut result = Self::RelacionalAnd(left.clone());

        if let Some(&token) = tokens.0.peek() {
            if *token == Token::OU {
                tokens.0.next();
                let right = ExpressaoRelacionalAnd::parse(tokens)?;
                
                result = Self::Struct { 
                    left: Box::new(result), 
                    op: Token::OU, 
                    right: Box::new(right),
                };
            }
        }
        
        match &result {
            ExpressaoRelacionalOu::RelacionalAnd(and) => match and {
                ExpressaoRelacionalAnd::Relacional(rl) => match rl {
                    ExpressaoRelacional::Primaria(_) => return None,
                    _ => (),
                },
                _ => (),
            },
            _ => (),
        }
        
        Some(result)
    }
}

impl ExpressaoRelacionalAnd {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self> 
    where 
        I: Clone + Iterator<Item = &'a Token>
    {
        let left = ExpressaoRelacional::parse(tokens)?;
        let mut result = Self::Relacional(left.clone());

        while let Some(&token) = tokens.0.peek() {
            if *token == Token::E {
                tokens.0.next();
                let right = ExpressaoRelacional::parse(tokens)?;
                
                result = Self::Struct { 
                    left: Box::new(result), 
                    op: Token::E, 
                    right: Box::new(right),
                };
            }
            else {
                break;
            }
        }
        
        Some(result)
    }
}

impl ExpressaoRelacional {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self> 
    where 
        I: Clone + Iterator<Item = &'a Token>
    {
        let mut left = None;

        let mut iter_clone = tokens.clone();
        match tokens.0.peek()? {
            Token::ABRE_PARENTESE => {
                iter_clone.0.next();
                
                if let Some(expr) = ExpressaoRelacionalOu::parse(&mut iter_clone) {
                    if let Some(Token::FECHA_PARENTESE) = iter_clone.0.next() {
                        *tokens = iter_clone;
                        return Some(Self::Nested(Box::new(expr)));
                    }
                    else {
                        return None;
                    }
                }
                else {
                    iter_clone = tokens.clone();

                    left = ExpressaoPrimaria::parse(&mut iter_clone);
                    *tokens = iter_clone;
                }
            }
            _ => ()
        }

        let left = match left {
            Some(val) => val,
            None => ExpressaoPrimaria::parse(tokens)?,
        };
        let mut result = Self::Primaria(left.clone());
        let mut op = None;

        let mut op_set_flag = false;

        let mut iter_clone = tokens.clone();
        while let Some(&token) = iter_clone.0.peek() {
            op = match token {
                Token::MAIOR
                | Token::MENOR
                | Token::MAIOR_IGUAL
                | Token::MENOR_IGUAL 
                | Token::DIFERENTE 
                | Token::IGUAL => {
                    if op_set_flag { return None; }
                    op_set_flag = true;
                    Some(token.clone())
                },
                _ => {
                    if let Some(op) = op {
                        let right = ExpressaoPrimaria::parse(&mut iter_clone)?;
                        result = Self::Struct { 
                            left: Box::new(left), 
                            op, 
                            right: Box::new(right),
                        };

                        *tokens = iter_clone;
                    }
                    break;
                }
            };
            
            iter_clone.0.next();
        }

        Some(result)
    }
}

impl ExpressaoAritmeticaAditiva {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let left = ExpressaoAritmeticaMultiplicativa::parse(tokens)?;
        let mut result = Self::Multiplicativa(left.clone());

        while let Some(&token) = tokens.0.peek() {
            if *token == Token::ADICAO || *token == Token::SUBTRACAO {
                tokens.0.next();
                let right = ExpressaoAritmeticaMultiplicativa::parse(tokens)?;

                result = Self::Operacao {
                    left: Box::new(result),
                    op: token.clone(),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        match &result {
            ExpressaoAritmeticaAditiva::Multiplicativa(mt) => match mt {
                ExpressaoAritmeticaMultiplicativa::Primaria(expr) => match expr {
                    ExpressaoPrimaria::Nested(_) => (),
                    _ => return None,
                },
                _ => (),
            },
            _ => (),
        }
        
        Some(result)
    }
}

impl ExpressaoAritmeticaMultiplicativa {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let left = ExpressaoPrimaria::parse(tokens)?;
        match &left {
            ExpressaoPrimaria::Const(tk) => match tk {
                Token::CONSTANTE_CHAR(_) => return None,
                _ => (),
            },
            _ => ()
        }
        let mut result = Self::Primaria(left.clone());

        while let Some(&token) = tokens.0.peek() {
            if *token == Token::MULTIPLICACAO
                || *token == Token::DIVISAO
                || *token == Token::MODULO
            {
                tokens.0.next();
                let right = ExpressaoPrimaria::parse(tokens)?;
                match &right {
                    ExpressaoPrimaria::Const(tk) => match tk {
                        Token::CONSTANTE_CHAR(_) => return None,
                        _ => (),
                    },
                    _ => ()
                }

                result = Self::Operacao {
                    left: Box::new(result),
                    op: token.clone(),
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }

        Some(result)
    }
}

impl Expressao {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let mut iter_clone = tokens.clone();
        if let Some(atrib) = ExpressaoAtribuicao::parse(&mut iter_clone) {
            *tokens = iter_clone;
            if let Some(Token::PONTO_VIRGULA) = tokens.0.next() {
                return Some(Self::Atribuicao(atrib));
            }
        }
        
        iter_clone = tokens.clone();
        if let Some(arit) = ExpressaoAritmeticaAditiva::parse(&mut iter_clone) {
            *tokens = iter_clone;
            if let Some(Token::PONTO_VIRGULA) = tokens.0.next() {
                return Some(Self::Aritmetica(arit));
            }
        }

        iter_clone = tokens.clone();
        if let Some(rela) = ExpressaoRelacionalOu::parse(&mut iter_clone) {
            *tokens = iter_clone;
            if let Some(Token::PONTO_VIRGULA) = tokens.0.next() {
                return Some(Self::Relacional(rela));
            }
        }

        None
    }
}

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// -------------------------------------- Declaracoes -------------------------------------
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#[derive(Debug, Clone)]
pub(super) enum Declaracao {
    Expressao(Expressao),
    Condicional(DeclaracaoCondicional),
    Loop(DeclaracaoLoop),
    Io(DeclaracaoIo),
}
impl Declaracao {
    pub(super) fn translate(&self) -> String {
        match self {
            Declaracao::Expressao(expr) => expr.translate(),
            Declaracao::Condicional(decl) => decl.translate(),
            Declaracao::Loop(decl) => decl.translate(),
            Declaracao::Io(decl) => decl.translate(),
        }
    }
}
impl std::fmt::Display for Declaracao {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaracao::Expressao(val) => write!(f, "!!{val}!!"),
            Declaracao::Condicional(val) => write!(f, "!!{val}!!"),
            Declaracao::Loop(val) => write!(f, "!!{val}!!"),
            Declaracao::Io(val) => write!(f, "!!{val}!!"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct DeclaracaoCondicionalIf {
    expr: ExpressaoRelacionalOu,
    decls: Vec<Declaracao>,
}
impl DeclaracaoCondicionalIf {
    pub(super) fn translate(&self) -> String {
        let body = std::format!("if ({}) {{", self.expr.translate());

        let mut decls = String::new();
        for d in &self.decls {
            decls = std::format!("{} {}", decls, d.translate());
        }
        
        std::format!("{} {} }}", body, decls)
    }
}
impl std::fmt::Display for DeclaracaoCondicionalIf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = write!(f, "IF({}), begin\n", self.expr);
        for d in &self.decls {
            println!("    {d}");
        }
        println!("end");
        
        result
    }
}

#[derive(Debug, Clone)]
pub(super) struct DeclaracaoCondicionalElse(Vec<Declaracao>);
impl DeclaracaoCondicionalElse {
    pub(super) fn translate(&self) -> String {
        let mut decls = String::new();
        for d in &self.0 {
            decls = std::format!("{} {}", decls, d.translate());
        }
        
        std::format!("else {{{}}}", decls)
    }
}
impl std::fmt::Display for DeclaracaoCondicionalElse {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        println!("ELSE begin");
        for d in &self.0 {
            println!("    {d}");
        }
        println!("end");
        
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeclaracaoCondicional {
    IF(DeclaracaoCondicionalIf),
    IfElse {
        _if: DeclaracaoCondicionalIf,
        _else: DeclaracaoCondicionalElse,
    }
}
impl DeclaracaoCondicional {
    pub(super) fn translate(&self) -> String {
        match self {
            DeclaracaoCondicional::IF(decl) => decl.translate(),
            DeclaracaoCondicional::IfElse { _if, _else } => std::format!("{} {}", _if.translate(), _else.translate()),
        }
    }
}
impl std::fmt::Display for DeclaracaoCondicional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclaracaoCondicional::IF(val) => write!(f, "{val}"),
            DeclaracaoCondicional::IfElse { _if, _else } => write!(f, "{_if}{_else}"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeclaracaoLoop {
    For {
        id: Token,
        begin: ExpressaoPrimaria,
        end: ExpressaoPrimaria,
        decls: Vec<Declaracao>,
    },
    While {
        condition: ExpressaoRelacionalOu,
        decls: Vec<Declaracao>,
    }
}
impl DeclaracaoLoop {
    pub(super) fn translate(&self) -> String {
        match self {
            DeclaracaoLoop::For { id, begin, end, decls } => {
                let mut decla = String::new();
                for d in decls {
                    decla = std::format!("{} {}", decla, d.translate());
                }

                std::format!("for (int {} = {}; {} < {}; {}+=1) {{{}}}", id.to_cpp(), begin.translate(), id.to_cpp(), end.translate(), id.to_cpp(), decla)
            },
            DeclaracaoLoop::While { condition, decls } => {
                let mut decla = String::new();
                for d in decls {
                    decla = std::format!("{} {}", decla, d.translate());
                }
                
                std::format!("while ({}) {{{}}}", condition.translate(), decla)
            },
        }
    }
}
impl std::fmt::Display for DeclaracaoLoop {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclaracaoLoop::For { id, begin, end, decls } => {
                println!("FOR {id} in {begin}..{end} begin");
                for d in decls {
                    println!("    {d}");
                }

                println!("end");
            },
            DeclaracaoLoop::While { condition, decls } => {
                println!("WHILE ({condition}) begin");
                for d in decls {
                    println!("    {d}");
                }
                
                println!("end");
            },
        }
        
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(super) enum DeclaracaoIo {
    Entrada {
        tipo: Token,
        id: Token,
    },
    Saida {
        tipo: Token,
        expr: ExpressaoPrimaria,
    }
}
impl DeclaracaoIo {
    pub(super) fn translate(&self) -> String {
        match self {
            DeclaracaoIo::Entrada { tipo, id } => {
                let tipo = match tipo {
                    Token::CHAR => "%c",
                    Token::INT32 => "%d",
                    Token::FLOAT32 => "%f",
                    
                    _ => panic!("Tipo especificado em read() eh invalido!"),
                };
                
                std::format!("scanf(\"{}\", &{});", tipo, id.to_cpp())
            },
            DeclaracaoIo::Saida { tipo, expr } => {
                let tipo = match tipo {
                    Token::CHAR => "%c",
                    Token::INT32 => "%d",
                    Token::FLOAT32 => "%f",
                    
                    _ => panic!("Tipo especificado em read() eh invalido!"),
                };
                
                std::format!("printf(\"{}\", {});", tipo, expr.translate())
            }
        }
    }
}
impl std::fmt::Display for DeclaracaoIo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclaracaoIo::Entrada { tipo, id } => write!(f, "ENTRADA({id}, {tipo})"),
            DeclaracaoIo::Saida { tipo, expr } => write!(f, "SAIDA({expr}, {tipo})"),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct EntryPoint(Vec<Declaracao>);
impl EntryPoint {
    pub(super) fn translate(&self) -> String {
        let mut decls = String::new();
        for d in &self.0 {
            decls = std::format!("{} {}", decls, d.translate());
        }

        std::format!("#include<stdio.h>\nint main(int argv, char** argc){{{} return 0;}}", decls)
    }
}
impl std::fmt::Display for EntryPoint {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        println!("MAIN begin");
        for d in &self.0 {
            println!("{d}");
        }
        
        println!("end");
        
        Ok(())
    }
}

impl Declaracao {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let mut iter_clone = tokens.clone();
        if let Some(expr) = Expressao::parse(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(Self::Expressao(expr));
        }
        
        iter_clone = tokens.clone();
        if let Some(condicional) = DeclaracaoCondicional::parse(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(Self::Condicional(condicional));
        }

        iter_clone = tokens.clone();
        if let Some(_loop) = DeclaracaoLoop::parse(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(Self::Loop(_loop));
        }

        iter_clone = tokens.clone();
        if let Some(io) = DeclaracaoIo::parse(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(Self::Io(io));
        }

        None
    }
}

impl DeclaracaoCondicionalIf {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let Some(Token::IF) = tokens.0.peek() {
            tokens.0.next();

            if let Some(Token::ABRE_PARENTESE) = tokens.0.next() {
                if let Some(expr) = ExpressaoRelacionalOu::parse(tokens) {
                    if let (
                        Some(Token::FECHA_PARENTESE),
                        Some(Token::INICIO_BLOCO)
                    ) = (tokens.0.next(), tokens.0.next()) {
                        let mut decls = Vec::new();

                        while let Some(decl) = Declaracao::parse(tokens) {
                            decls.push(decl);
                        }
                        
                        if let Some(Token::FIM_BLOCO) = tokens.0.next() {
                            return Some(Self {
                                expr,
                                decls
                            })
                        }
                    }
                }
            }
        }

        None
    }
}

impl DeclaracaoCondicionalElse {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::ELSE),
            Some(Token::INICIO_BLOCO)
        ) = (tokens.0.next(), tokens.0.next()) {
            let mut decls = Vec::new();
            
            while let Some(decl) = Declaracao::parse(tokens) {
                decls.push(decl);
            }
            
            if let Some(Token::FIM_BLOCO) = tokens.0.next() {
                return Some(Self(decls));
            }
        }
        
        None
    }
}

impl DeclaracaoCondicional {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let _if = DeclaracaoCondicionalIf::parse(tokens)?;
        
        let mut iter_clone = tokens.clone();
        if let Some(_else) = DeclaracaoCondicionalElse::parse(&mut iter_clone) {
            *tokens = iter_clone;

            return Some(Self::IfElse { _if, _else });
        }

        Some(Self::IF(_if))
    }
}

impl DeclaracaoLoop {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let mut iter_clone = tokens.clone();
        
        if let Some(_for) = Self::parse_for(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(_for);
        }
        
        iter_clone = tokens.clone();
        if let Some(_while) = Self::parse_while(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(_while);
        }

        None
    }

    fn parse_for<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::FOR),
            Some(Token::ABRE_PARENTESE),
            Some(Token::IDENTIFICADOR(id)),
            Some(Token::DOIS_PONTOS),
        ) = (
            tokens.0.next(),
            tokens.0.next(),
            tokens.0.next(),
            tokens.0.next(),
        ) {
            let begin = ExpressaoPrimaria::parse(tokens)?;
            if let (
                Some(Token::PONTO),
                Some(Token::PONTO)
            ) = (
                tokens.0.next(),
                tokens.0.next(),
            ) {
                let end = ExpressaoPrimaria::parse(tokens)?;
                if let (
                    Some(Token::FECHA_PARENTESE),
                    Some(Token::INICIO_BLOCO),
                ) = (
                    tokens.0.next(),
                    tokens.0.next(),
                ) {
                    let mut decls = Vec::new();
                    let mut iter_clone = tokens.clone();

                    while let Some(decl) = Declaracao::parse(&mut iter_clone) {
                        *tokens = iter_clone.clone();
                        decls.push(decl);
                    }

                    if let Some(Token::FIM_BLOCO) = tokens.0.next() {
                        return Some(Self::For { 
                            id: Token::IDENTIFICADOR(id.clone()), 
                            begin, 
                            end, 
                            decls,
                        });
                    }
                }
            }
        }
         
        None
    }

    fn parse_while<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::WHILE),
            Some(Token::ABRE_PARENTESE),
        ) = (
            tokens.0.next(),
            tokens.0.next(),
        ) {
            let condition = ExpressaoRelacionalOu::parse(tokens)?;
            
            if let (
                Some(Token::FECHA_PARENTESE),
                Some(Token::INICIO_BLOCO),
            ) = (
                tokens.0.next(),
                tokens.0.next(),
            ) {
                let mut decls = Vec::new();
                let mut iter_clone = tokens.clone();
                
                while let Some(decl) = Declaracao::parse(&mut iter_clone) {
                    *tokens = iter_clone.clone();
                    decls.push(decl);
                }
                
                if let Some(Token::FIM_BLOCO) = tokens.0.next() {
                    return Some(Self::While { 
                        condition, 
                        decls
                    });
                }
            }
        }

        None
    }
}

impl DeclaracaoIo {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        let mut iter_clone = tokens.clone();
        if let Some(entrada) = Self::parse_entrada(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(entrada);
        }

        iter_clone = tokens.clone();
        if let Some(saida) = Self::parse_saida(&mut iter_clone) {
            *tokens = iter_clone;
            return Some(saida);
        }

        None
    }
   
    fn parse_entrada<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::ENTRADA),
            Some(Token::ABRE_PARENTESE),
            Some(Token::IDENTIFICADOR(id)),
            Some(Token::VIRGULA),
        ) = (
            tokens.0.next(),
            tokens.0.next(),
            tokens.0.next(),
            tokens.0.next(),
        ) {
            if let Some(tipo) = tokens.0.next() {
                let tipo = match tipo {
                    Token::CHAR
                    | Token::INT32
                    | Token::FLOAT32 => tipo.clone(),

                    _ => return None,
                };
                
                if let (
                    Some(Token::FECHA_PARENTESE),
                    Some(Token::PONTO_VIRGULA)
                ) = (
                    tokens.0.next(),
                    tokens.0.next()
                ) {
                    return Some(Self::Entrada { 
                        tipo, 
                        id: Token::IDENTIFICADOR(id.clone()) 
                    });
                }
            }
        }

        None
    }

    fn parse_saida<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::SAIDA),
            Some(Token::ABRE_PARENTESE),
        ) = (
            tokens.0.next(),
            tokens.0.next(),
        ) {
            let expr = ExpressaoPrimaria::parse(tokens)?;
            
            if let (
                Some(Token::VIRGULA),
                tipo,
                Some(Token::FECHA_PARENTESE),
                Some(Token::PONTO_VIRGULA),
            ) = (
                tokens.0.next(),
                tokens.0.next(),
                tokens.0.next(),
                tokens.0.next(),
            ) {
                let tipo = match tipo? {
                    Token::CHAR
                    | Token::INT32
                    | Token::FLOAT32 => tipo?.clone(),

                    _ => return None,
                };
                
                return Some(Self::Saida { 
                    tipo, 
                    expr 
                });
            }
        }

        None
    }
}

impl EntryPoint {
    pub(super) fn parse<'a, I>(tokens: &mut TokenIter<'a, I>) -> Option<Self>
    where
        I: Clone + Iterator<Item = &'a Token>,
    {
        if let (
            Some(Token::COMECO),
            Some(Token::INICIO_BLOCO),
        ) = (
            tokens.0.next(),
            tokens.0.next(),
        ) {
            let mut decls = Vec::new();
            let mut iter_clone = tokens.clone();
            
            while let Some(decl) = Declaracao::parse(&mut iter_clone) {
                *tokens = iter_clone.clone();
                decls.push(decl);
            }
            
            if let Some(Token::FIM_BLOCO) = tokens.0.next() {
                return Some(Self(decls));
            }
        }
        
        None
    }
}