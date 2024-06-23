Modo de utilização:
-   Em caso de primeira compilação será necessário a ferramenta cargo disponível em: https://www.rust-lang.org/pt-BR/learn/get-started, 
    então entrar com o comando cargo run <arquivo.rmm> para compilar o arquivo .rmm desejado.

- Caso o compilador já tenha sido compilado basta localizar o .exe (target/(debug/release)/rmm.exe) e entrar com o comando rmm.exe <arquivo.rmm> 
    para compilar o arquivo desejado.
    É possivel tambem utilizar o método anterior caso deseje.
    
Resultado:
-   O resultado da compilação gera uma pasta chamada rmm_target, onde se encontra os arquivos .c e .exe resultantes da compilação, bem como outra pasta
    chamada simbolos_gerados, onde se encontra os arquivos contendo informação a respeito dos tokens e expressoes/declarações geradas.