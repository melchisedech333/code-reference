
// Santa Maria <3

fn main() {
    
    // Declaramos uma string que pertence ao escopo do main.
    let mut s1 = String::from("Iesus");
    println!("s1: {s1}");

    // Passamos uma referência mutável como parâmetro.
    change_data(&mut s1);

    // Exibe valor alterado da variável.
    println!("s1: {s1}");
}

// A função aceita uma referência mutável como parâmetro.
fn change_data (s: &mut String) {

    // O valor da variável é alterado.
    s.push_str(" Salvator");
}


