
# Rust

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Hello World
- Cargo

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

Para compilar e executar.

```bash
cd hello_world
cargo build
./target/debug/hello_world
```


