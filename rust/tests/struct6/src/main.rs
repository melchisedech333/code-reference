
// Iesus <3

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {

    // Método que retorna uma nova instância.
    fn make_new() -> Self {
        Self {
            width: 10,
            height: 10,
        }
    }

    // Retorna apenas um número.
    fn make_num () -> u32 {
        333
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


