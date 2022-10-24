
/*
** IHS s2
*/

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


