
// IHS.

fn main() {
    let x = 5;

    // Como se trata de um número inteiro, o próprio dado
    // é simplesmente copiado para dentro da função. 
    makes_copy(x);

    // Como o dado foi copiado, isto continua válido.
    println!("x value: {x}");
}

fn makes_copy(some_integer: i32) {
    println!("copy data: {}", some_integer);
}

