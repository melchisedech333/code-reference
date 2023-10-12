
// Santíssima Trindade s2

fn main() {
    let a = [ 1, 2, 3, 4, 5 ];
    
    // Produz um slice (fatia) dos dados do array.
    let s = &a[1..3];

    // Percorre o novo array (slice).
    // Imprime:
    //    number: 2
    //    number: 3
    
    for number in s {
        println!("number: {}", number);
    }
}


