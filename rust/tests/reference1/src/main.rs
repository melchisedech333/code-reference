
// Santa Maria <3

fn main() {

    // s1 existe no escopo do main.
    let s1 = String::from("hello");

    // s1 é passada como referência para a função.
    let len = calculate_length(&s1);

    // s1 existe ainda no escopo do main, pois não foi movida para a função.
    println!("The length of '{}' is {}.", s1, len);
}

// A função aceita uma referência (deste tipo definido no protótipo)
fn calculate_length(s: &String) -> usize {

    // Observe que a realização da leitura dos dados é permitida.
    println!("Str: {s}");

    // Observe que ao finalizar o escopo do código, 's' não deixará de existir.
    // Pois a mesma não foi movida, mas apenas passada uma referência.
    s.len()
}


