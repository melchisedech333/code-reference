
use std::thread;
use std::sync::mpsc;

fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let val = String::from("Hello");
        tx.send(val).unwrap();
    });

    let received = rx.recv().unwrap();
    println!("Received: {}", received);
}


