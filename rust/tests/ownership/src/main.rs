
// I Love Trindade!

fn main() {

    // Testes string.
    let str1 = "str 1";
    let mut str2 = String::from("str 2");

    println!("str1: {str1}");
    println!("str2: {str2}");

    str2.push_str(" xD");
    println!("str3: {str2}");

    // Data copy.
    let x = 5;
    let y = x;

    println!("y: {y}, x: {x}");

    // move.
    let s1 = String::from("hello");
    let s2 = s1;

    println!("{}, world!", s2);

    // clone.
    let s1 = String::from("hello");
    let s2 = s1.clone();

    println!("s1 = {}, s2 = {}", s1, s2);
}


