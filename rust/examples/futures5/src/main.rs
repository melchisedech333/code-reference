
use futures::executor::block_on;
use std::{thread, time};

async fn task_1() {
    for _ in 1..5 {
        println!("task 1");
        thread::sleep(time::Duration::from_millis(100));
    }
}

async fn task_2() {
    for _ in 1..5 {
        println!("task 2");
        thread::sleep(time::Duration::from_millis(10));
    }
}

async fn run_all_tasks() {
    let t1 = task_1();

    for _ in 1..3 {
        println!("main...");
    }

    let t2 = task_2();

    t2.await;
    t1.await;
}

fn main() {
    block_on(run_all_tasks());
}


