#[allow(non_camel_case_types)]

mod tokenizer;
pub(crate) mod parser;

pub(crate) use tokenizer::tokenize;

const COMECO: &str = "main";
const INT32: &str = "i32";
const FLOAT32: &str = "f32";
const CHAR: &str = "char";
const FOR: &str = "for";
const WHILE: &str = "while";
const ENTRADA: &str = "read";
const SAIDA: &str = "write";
const INICIO_BLOCO: &str = "begin";
const FIM_BLOCO: &str = "end";
const IF: &str = "?";
const ELSE: &str = "~?";
const ATRIBUICAO: &str = "->";
const IGUAL: &str = "=";
const DIFERENTE: &str = "~=";
const MAIOR: &str = ">";
const MAIOR_IGUAL: &str = "+=";
const MENOR: &str = "<";
const MENOR_IGUAL: &str = "-=";
const E: &str = "&";
const OU: &str = "|";
const NEGACAO: &str = "~";
const ADICAO: &str = "+";
const SUBTRACAO: &str = "-";
const MULTIPLICACAO: &str = "*";
const DIVISAO: &str = "/";
const MODULO: &str = "%";
const PONTO: &str = ".";
const DOIS_PONTOS: &str = ":";
const VIRGULA: &str = ",";
const PONTO_VIRGULA: &str = ";";
const ASPAS_SIMPLES: &str = "'";
const ABRE_PARENTESE: &str = "(";
const FECHA_PARENTESE: &str = ")";

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    COMECO,
    
    CHAR,
    INT32,
    FLOAT32,

    FOR,
    WHILE,

    ENTRADA,
    SAIDA,

    INICIO_BLOCO,
    FIM_BLOCO,

    IF,
    ELSE,

    ATRIBUICAO,

    IGUAL,
    DIFERENTE,
    MAIOR,
    MAIOR_IGUAL,
    MENOR,
    MENOR_IGUAL,

    E,
    OU,
    NEGACAO,
    
    ADICAO,
    SUBTRACAO,
    MULTIPLICACAO,
    DIVISAO,
    MODULO,
    
    PONTO,
    DOIS_PONTOS,
    VIRGULA,
    PONTO_VIRGULA,
    ASPAS_SIMPLES,
    
    ABRE_PARENTESE,
    FECHA_PARENTESE,

    CONSTANTE_INT(String),
    CONSTANTE_FLOAT(String),
    CONSTANTE_CHAR(String),
    IDENTIFICADOR(String),
}
impl Token {
    pub(crate) fn get(valor: &str) -> Option<Self> {
        match valor {
            COMECO =>       Some(Self::COMECO),

            INT32 =>        Some(Self::INT32),
            FLOAT32 =>      Some(Self::FLOAT32),
            CHAR =>         Some(Self::CHAR),

            FOR =>          Some(Self::FOR),
            WHILE =>        Some(Self::WHILE),

            ENTRADA =>      Some(Self::ENTRADA),
            SAIDA =>        Some(Self::SAIDA),

            INICIO_BLOCO => Some(Self::INICIO_BLOCO),
            FIM_BLOCO =>    Some(Self::FIM_BLOCO),

            IF =>           Some(Self::IF),
            ELSE =>         Some(Self::ELSE),
            
            ATRIBUICAO =>   Some(Self::ATRIBUICAO),
            
            IGUAL =>        Some(Self::IGUAL),
            DIFERENTE =>    Some(Self::DIFERENTE),
            MAIOR =>        Some(Self::MAIOR),
            MAIOR_IGUAL =>  Some(Self::MAIOR_IGUAL),
            MENOR =>        Some(Self::MENOR),
            MENOR_IGUAL =>  Some(Self::MENOR_IGUAL),

            E =>            Some(Self::E),
            OU =>           Some(Self::OU),
            NEGACAO =>      Some(Self::NEGACAO),

            ADICAO =>       Some(Self::ADICAO),
            SUBTRACAO =>    Some(Self::SUBTRACAO),
            MULTIPLICACAO => Some(Self::MULTIPLICACAO),
            DIVISAO =>      Some(Self::DIVISAO),
            MODULO =>       Some(Self::MODULO),

            PONTO =>        Some(Self::PONTO),
            DOIS_PONTOS =>  Some(Self::DOIS_PONTOS),
            VIRGULA =>      Some(Self::VIRGULA),
            PONTO_VIRGULA => Some(Self::PONTO_VIRGULA),
            ASPAS_SIMPLES => Some(Self::ASPAS_SIMPLES),
            ABRE_PARENTESE => Some(Self::ABRE_PARENTESE),
            FECHA_PARENTESE => Some(Self::FECHA_PARENTESE),

            _ => {
                let valor = valor.to_string();
                if valor.chars().next().unwrap().is_ascii_digit() { 
                    if valor.contains(".") {
                        Some(Self::CONSTANTE_FLOAT(valor))
                    }
                    else {
                        Some(Self::CONSTANTE_INT(valor))
                    }
                }
                else if valor.chars().next().unwrap() == '\'' && valor.chars().next_back().unwrap() == '\'' {
                    Some(Self::CONSTANTE_CHAR(valor))
                }
                else if valor.chars().next().unwrap().is_ascii_alphabetic() {
                    Some(Self::IDENTIFICADOR(std::format!("{}_", valor)))
                }
                else { 
                    None
                }
            }
        }
    }
    pub(crate) fn to_cpp(&self) -> String {
        match &self {
            Token::COMECO => "int main()",
            Token::CHAR => "char",
            Token::INT32 => "int",
            Token::FLOAT32 => "float",
            Token::FOR => "for",
            Token::WHILE => "while",
            Token::ENTRADA => todo!(),
            Token::SAIDA => todo!(),
            Token::INICIO_BLOCO => "{",
            Token::FIM_BLOCO => "}",
            Token::IF => "if",
            Token::ELSE => "else",
            Token::ATRIBUICAO => "=",
            Token::IGUAL => "==",
            Token::DIFERENTE => "!=",
            Token::MAIOR => ">",
            Token::MAIOR_IGUAL => ">=",
            Token::MENOR => "<",
            Token::MENOR_IGUAL => "<=",
            Token::E => "&&",
            Token::OU => "||",
            Token::NEGACAO => "!",
            Token::ADICAO => ADICAO,
            Token::SUBTRACAO => SUBTRACAO,
            Token::MULTIPLICACAO => MULTIPLICACAO,
            Token::DIVISAO => DIVISAO,
            Token::MODULO => MODULO,
            Token::PONTO => PONTO,
            Token::DOIS_PONTOS => DOIS_PONTOS,
            Token::VIRGULA => VIRGULA,
            Token::PONTO_VIRGULA => PONTO_VIRGULA,
            Token::ASPAS_SIMPLES => ASPAS_SIMPLES,
            Token::ABRE_PARENTESE => ABRE_PARENTESE,
            Token::FECHA_PARENTESE => FECHA_PARENTESE,
            Token::CONSTANTE_INT(val) => val,
            Token::CONSTANTE_FLOAT(val) => val,
            Token::CONSTANTE_CHAR(val) => val,
            Token::IDENTIFICADOR(val) => val,
        }.to_string()
    }
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CONSTANTE_CHAR(val) 
            | Self::CONSTANTE_INT(val)
            | Self::CONSTANTE_FLOAT(val)
            | Self::IDENTIFICADOR(val) => write!(f, "{val}"),
            _ => write!(f, "{:?}", self),
        }
    }
}