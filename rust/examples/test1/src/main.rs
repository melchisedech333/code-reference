
fn main() {
    let mut num = 10;
    num = add(num, 20);
    
    println!("value: {num}");
}

// Define this in a crate called `adder`.
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[test]
fn test_add() {
    assert_eq!(add(3, 2), 5);
}


