use super::Token;

pub(crate) fn tokenize(conteudo: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut conteudo_iter = conteudo.chars().peekable();
    let mut buffer: String = String::default();

    // Percorrer a string inteira
    while let Some(letra) = conteudo_iter.next() {
        // Caso o char for espaco
        if letra.is_whitespace() { continue; }

        if letra.is_ascii_alphabetic() { // IDENTIFICADOR e palavras reservadas
            buffer.push(letra);

            // Podemos continuar a busca verificando se os subsequentes
            // caracteres sao letras ou numeros
            while let Some(&sub_letra) = conteudo_iter.peek() {
                if sub_letra.is_ascii_alphanumeric() {
                    buffer.push(sub_letra);
                    conteudo_iter.next();
                }
                else {
                    break;
                }
            }
        } 
        else if letra.is_ascii_digit() { // CONSTANTE_INT e CONSTANTE_FLOAT
            buffer.push(letra);

            let mut ponto = ' ';
            let mut iter_clone = conteudo_iter.clone();

            while let Some(&sub_letra) = iter_clone.peek() {
                if sub_letra == '.' {
                    if ponto == '.' {
                        break;
                    } else {
                        ponto = sub_letra;
                        iter_clone.next();
                    }
                } 
                else if sub_letra.is_ascii_digit() {
                    if ponto == '.' && !buffer.contains(".") {
                        buffer.push(ponto);
                    }
                    buffer.push(sub_letra);

                    iter_clone.next();
                    conteudo_iter = iter_clone.clone();
                }
                else { break; }
            }
        }
        else if letra == '\'' { // CONSTANTE_CHAR
            buffer.push(letra);
            let mut sub_buffer = buffer.clone();
            
            let mut iter_clone = conteudo_iter.clone();
            for _ in 0..2 {
                if let Some(letra) = iter_clone.next() {
                    sub_buffer.push(letra);
                } 
                else { break; }
            }

            if Token::get(&sub_buffer).is_some() {
                buffer = sub_buffer;
                conteudo_iter = iter_clone;
            }
        }
        else if !letra.is_ascii_whitespace() { // Outros simbolos
            buffer.push(letra);
            let mut sub_buffer = buffer.clone();

            while let Some(&letra) = conteudo_iter.peek() {
                sub_buffer.push(letra);
                if Token::get(&sub_buffer).is_some() {
                    buffer.push(letra);
                }
                else { break; }
                
                conteudo_iter.next();
            }
        }
        
        // Caso a sintaxe esteja correta o token sera adicionado na lista de tokens
        if buffer.is_empty() { continue; }
        if let Some(token) = Token::get(&buffer) {
            tokens.push(token);
        } 
        else {
            panic!("Sintaxe invalida em: {}", buffer);
        }

        buffer.clear();
    }
    
    tokens
}