
// I Love Christ s2

fn main() {
    let mut x = 5;
    println!("The value of x is: {x}");

    x = 6;
    println!("The value of x is: {x}");

    // Sombreamento.
    let x = 5;

    let x = x + 1;

    {
        let x = x * 2;
        println!("The value of x in the inner scope is: {x}");
    }

    println!("The value of x is: {x}");

    // Outro caso de sombreamento.
    let spaces = "   ";
    let spaces = spaces.len();

    println!("Spaces: {spaces}");

    // Números.
    let num1 = 1_333;
    let num2 = 1333;
    
    println!("Num1: {num1} - Num2: {num2}"); // Imprime 1333

    let num3 = 1.333;

    println!("Num3: {num3}"); // Imprime 1.333

    // Caracteres.
    let heart_eyed_cat = '😻';

    println!("Cat: {heart_eyed_cat}");

    // Tuplas.
    let tup = (500, 6.4, 1);

    let (_x, _y, _z) = tup;

    println!("The value of y is: {_y}");

    // Acesso a elementos.
    let x: (i32, f64, u8) = (500, 6.4, 1);

    println!("1: {}, 2: {}, 3: {}", x.0, x.1, x.2);

    // -> Array.

    // Numérico.
    let a = [1, 2, 3, 4, 5];

    // Strings.
    let months = ["January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December"];

    println!("ar1: {}, ar2: {}", a[2], months[8]);
}


