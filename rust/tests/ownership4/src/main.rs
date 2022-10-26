
// IHS =)

fn main() {

    // Armazena em s1, a string criada dentro da função gives_ownership().
    let s1 = gives_ownership();
    let s2 = String::from("hello");
    
    // Move s2 para dentro da função.
    let s3 = takes_and_gives_back(s2);
    
    println!("s1: {s1}");

    // Esta linha produz um erro.
    // Pois s2 foi movida para dentro da função takes_and_gives_back().
    // println!("s2: {s2}");

    println!("s3: {s3}");

} // A memória de s1 e s3 são liberadas. Já com s2 não é feito nada, pois ele
  // foi movido. 

fn gives_ownership() -> String {
    let some_string = String::from("yours");

    // Retorna a string que foi criada.
    some_string
}

fn takes_and_gives_back(a_string: String) -> String {
    a_string  // move a string para quem chamou a função.
}


