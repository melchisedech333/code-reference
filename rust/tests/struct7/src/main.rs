
// Iesus <3

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

fn main() {
    let rect1 = Rectangle::make_new();
    let mut rect2 = Rectangle::make_new();
    let num  = Rectangle::make_num();

    // Modificando a instância.
    rect2.width  = 30;
    rect2.height = 30;

    println!("Rect 1: {}, {}", rect1.width, rect1.height);
    println!("Rect 2: {}, {}", rect2.width, rect2.height);
    println!("Num...: {}", num);
}


