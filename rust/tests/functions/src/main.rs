
// Iesus s2

fn main() {
    another_function(5);
    print_labeled_measurement(5, 'h');

    // Este bloco de código é uma expressão e retorna o valor 4.
    let y = {
        let x = 3;
        x + 1
    };

    println!("The value of y is: {y}");

    // Funções com retorno de valores.
    let a = func1();
    println!("a: {a}");

    let b = func2();
    println!("b: {b}");

    // Add 1.
    let x = plus_one(5);
    println!("The value of x is: {x}"); // Imprime 6
}

// Único parâmetro.
//                  nome:  tipo
fn another_function(value: i32) {
    println!("Value: {value}");
}

// Múltiplos parâmetros.
fn print_labeled_measurement(value: i32, unit_label: char) {
    println!("The measurement is: {value}{unit_label}");
}

fn func1() -> i32 {
    333
}

fn func2() -> i32 {

    println!("Call func2()");
    return 111;

    333
}

fn plus_one(x: i32) -> i32 {
    x + 1
}


