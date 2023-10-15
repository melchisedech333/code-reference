
//! # Doc 1 Exemplo
//! 
//! Este é meu arquivo main. Nele deve ficar a explicação geral do projeto.
//! 
//! **Opções:**
//! - Opção 1
//! - *Opção 2*
//! - `Opção 2`
//! - [`Link externo Google`]
//!
//! # Examples
//!
//! ```
//! use std::env;
//!
//! // Prints each argument on a separate line
//! for argument in env::args() {
//!     println!("{argument}");
//! }
//! ```
//! 
//! *Italic*
//! 
//! **Bold**
//! 
//! # Heading 1
//! 
//! Heading 1
//! =========
//! 
//! ## Heading 2
//! 
//! Heading 2
//! ---------
//!
//! [Link](http://a.com)
//!
//! ![Image](../static.files/rust-logo-151179464ae7ed46.svg) 
//! 
//! > Blockquote 1
//! 
//! > Blockquote 2
//! 
//! > Blockquote 3
//! 
//! - List
//! - List
//! - List
//! 
//! 1. One
//! 2. Two
//! 3. Three
//! 
//! ---
//! 
//! Teste 1
//! 
//! ---
//! 
//! Teste 2
//! 
//! ---
//! 
//! `Inline code` with backticks
//! 
//! ```rust
//! println!("IHS s2");
//! example();
//! ```
//! 
//! [`Link externo Google`]: https://google.com
//! [`args_os`]: ./fn.args_os.html

// #![doc(html_logo_url = "../../../docs-custom/logo.png")]
#[doc = include_str!("../docs-custom/module.md")]

mod module;
use module::*;

#[doc = include_str!("../docs-custom/main.md")]

/// # Ponto de inicialização.
/// 
/// É aqui que a aplicação inicia e faz todos os paranaues.
/// Depois ela continua executando...
/// Esta é uma documentação externa.
/// 
/// E depois também rsrs...
/// 
/// ## Exemplo:
/// 
/// Segue abaixo um exemplo de código.
/// Usando **negrito** e outras *coisitas*.
/// 
/// - Ponto 1
/// - Ponto 2
/// - Ponto 3
/// 
/// ```rust
/// println!("IHS s2");
/// example();
/// ```
/// 

// Esta é uma documentação interna...
// E nada aqui aparecerá na documentação externa.

fn main() {
    println!("Hello, world!");

    // Comentário de exemplo...
    example();
}


