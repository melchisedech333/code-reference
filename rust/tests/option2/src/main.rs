
// IHS s2

fn main() {
    let five = Some(5);
    let six = plus_one(five);

    if six != None {
        println!("six != None"); // Imprime.
    } else {
        println!("six = None");
    }

    // Passa "None" como parâmetro.
    let none = plus_one(None);

    if none != None {
        println!("none != None");
    } else {
        println!("none = None"); // Imprime.
    }
}

fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1), // Retorna o valor incrementando em 1.
    }
}


