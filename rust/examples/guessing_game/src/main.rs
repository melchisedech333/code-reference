
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


