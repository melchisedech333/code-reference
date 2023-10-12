
// IHS.

fn main() {
    let s = String::from("hello");  // s existe dentro do escopo do main

    takes_ownership(s);             // 's' é movida (move) para dentro da função.
                                    // ... e daqui para baixo ela não existe mais.

    // Esta linha de código produz um erro.
    // println!("s value: {s}");

} // Após finalizar o main(), como 's' foi movida, nada especial acontece.

fn takes_ownership(some_string: String) {
    println!("{} world!", some_string);
} // Como agora 's' foi movida para cá, após terminar este 
  // escopo, a memória será liberada.


