
# Rust

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Hello World
- Cargo
- Arquivos de cabeçalho
- Formatação de strings
- Variáveis
- Outros exemplos

Faltantes:
- Loop
- Funções
- Comparação números e strings

<br>

***

## Hello World


```rust
fn main() {
    println!("Hello, world!");
}
```

<br>

Para compilar e executar.

```bash
rustc hello_world.rs
./hello_world
```

<br>

***

## Cargo

<br>

Ao utilizar o comando abaixo, será criado a estrutura básica de um projeto, e o diretório será automaticamente inicializado com git.

<b>Obs:</b> se o cargo for utilizado dentro de um diretório git já previamente inicializado, o novo diretório criado não será inicializado com git.

```bash
cargo new hello_world
```

<br>
Para gerar a estrutura do projeto, sem gerar os arquivos git.

```bash
cargo new hello_world --vcs=none
```

<br>

Diretórios e arquivos criados:

```bash
hello_world/         # Diretório raiz.
    Cargo.toml       # Arquivo de configuração.
    src/main.rs      # Arquivo Rust.
    .gitignore       # Git files.
    .git             # Git files.
```

<br>

Para compilar e executar:

```bash
cd hello_world
cargo build
./target/debug/hello_world
```

Para fazer isso automático:

```bash
cargo run
```

Para realizar uma verificação prévia, para garantir que todo o código está correto para ser compilado:

```bash
cargo check
```

<br>

<b>Dependências:</b> ao inserir uma dependência no arquivo <i>Cargo.toml</i>, basta digitar <i>cargo build</i>, pois elas serão baixadas e instaladas.

Os pacotes do Rust, criados pela comunidade ficam aqui: https://crates.io/

<br>

Para abrir a documentação de dependẽncias basta digitar o comando abaixo. Ele abrirá a documentação localmente no Browser, basta selecionar a de interesse.

```bash
cargo doc --open
```

<br>

***

## Arquivos de cabeçalho

<br>

```rust
use std:io;
```

<br>

***

## Formatação de strings

<br>

Os dois modos abaixo funcionam.

```rust
let a = 5;
let b = 10;

println!("1, a = {a}, b = {b}");
println!("2, a = {}, b = {}", a, b);
```

<br>

***

## Variáveis

<br>

Toda variável em Rust é imutável:

```rust

```

<br>

***

## Outros exemplos

<br>

Jogo de advinhação.

```rust
/*
** IHS s2
*/

// Importa módulos.
use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    // Gera um número aleatório de 1 a 100.
    let secret_number = rand::thread_rng().gen_range(1..=100);

    loop {
        println!("Enter input.");

        // Define uma variável mutável.
        let mut guess = String::new();
        
        // Realiza leitura da entrada padrão (terminal).
        io::stdin()

            /* Passa como parâmetro a variável.
            * Onde & é uma referência à mesma.
            * E "mut" explicita que ela é mutável. */
            .read_line(&mut guess)
            .expect("Failed to read line.");

        // Converte a string para número inteiro.
        // Obs: foi realizado um "shadow" na variável, podendo reutilizar
        // o mesmo nome de uma variável já anteriormente declarada.
        //
        // Forma utilizando expect.
        // let guess :u32 = guess.trim().parse().expect("Please type a number.");
        //
        // Forma utilizando match, para realizar o tratamento da entrada.
        // Caso obter sucesso, retorna o próprio número.
        // Caso contrário, executa o continue no loop do escopo em questão.
        let guess :u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {guess}");

        // Compara os valores.
        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),

            // Caso o número for igual, finaliza o jogo.
            Ordering::Equal   => {
                println!("You win!");
                break;
            }
        }
    }
}
```


