mod rmm_core;

use std::{io::{Read, Write}, process::Command};

use rmm_core::parser::Parser;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    if let Some(arquivo) = args.get(1) {
        let path = std::path::Path::new(arquivo);
        match path.extension() {
            Some(ext) => if ext.to_str().unwrap() != "rmm" {
                panic!("O arquivo especificado nao é um arquivo rmm!");
            },
            None => panic!("O arquivo especificado nao é um arquivo rmm!"),
        }

        let mut arquivo = std::fs::File::open(path)
            .expect("Arquivo especificado nao encontrado!");

        let mut conteudo = String::default();
        arquivo.read_to_string(&mut conteudo)
            .expect("Arquivo especificado possui caracteres invalidos!");
        

        // Compilando
        let tokens = rmm_core::tokenize(&conteudo);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let conteudo = parser.translate();

        // Criando o arquivo .c
        let rmm_file_name = path.file_stem().unwrap().to_str().unwrap();
        let c_file_name = std::format!("{}.c", rmm_file_name);
        let c_dir_path = "rmm_target";

        let output_file_path = std::format!("{}/{}.exe", c_dir_path, rmm_file_name);
        let compile_path = std::format!("{}/{}", c_dir_path, c_file_name);

        std::fs::create_dir_all(c_dir_path).unwrap();
        
        let mut c_file = std::fs::File::create(compile_path.clone()).unwrap();
        c_file.write_all(conteudo.as_bytes()).unwrap();

        let status = Command::new("gcc")
            .arg(compile_path)        
            .arg("-o")
            .arg(output_file_path)
            .status().unwrap();
        
        if !status.success() {
            panic!("Failed to compile .c file");
        }
    } else {
        panic!("Compilador sem argumentos! Especifique o arquivo a ser compilado!");
    }
}