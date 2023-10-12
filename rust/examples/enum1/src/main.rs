
// IHS <3

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) -> u32 {
        // method body would be defined here
        333
    }
}

fn main() {
    let m = Message::Write(String::from("hello"));
    println!("Value: {}", m.call());
}


