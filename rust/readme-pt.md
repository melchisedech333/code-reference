
# Rust

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Hello World
- Cargo
- Arquivos de cabeçalho
- Variáveis
- Concatenação

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

***

## Arquivos de cabeçalho

<br>

```rust
use std:io;
```

<br>

***

## Variáveis

<br>

Toda variável em Rust é imutável:

```rust
let test = "";
```

<br>

Exemplo utilizando variáveis.

```rust
// Importa módulos.
use std::io;

fn main() {
    println!("Enter: ");

    // Define uma variável mutável.
    let mut guess = String::new();

    io::stdin()

        /* Passa como parâmetro a variável.
         * Onde & é uma referência à mesma.
         * E "mut" explicita que ela é mutável. */
        .read_line(&mut guess)
        .expect("Failed to read line.");

    println!("You guessed: {guess}");
}
```

<br>

***

## Concatenação

<br>

Os dois modos abaixo funcionam.

```rust
let a = 5;
let b = 10;

println!("1, a = {a}, b = {b}");
println!("2, a = {}, b = {}", a, b);
```


