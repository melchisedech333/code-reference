
//! # Este é meu módulo.
//!
//! **Opções:**
//! - Opção 1
//! - *Opção 2*
//! - `Opção 2`

/// This struct is not [Bar]
pub struct Foo1;

/// This struct is also not [bar](Bar)
pub struct Foo2;

/// This struct is also not [bar][b]
///
/// [b]: Bar
pub struct Foo3;

/// This struct is also not [`Bar`]
pub struct Foo4;

/// This struct *is* [`Bar`]!
pub struct Bar;

#[doc = include_str!("../docs-custom/function-1.md")]

/// # Função de exemplo.
/// 
/// IHS s2
/// 

// Documentação interna.

pub fn example() {
    println!("module.example");
}


