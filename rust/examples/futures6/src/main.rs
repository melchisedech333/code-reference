
use std::{thread, time};
use futures::executor::block_on;
use futures::future::join_all;
use async_std::task;

async fn task_1() {
    for _ in 1..5 {
        println!("task 1");
        thread::sleep(time::Duration::from_millis(100));
    }
}

async fn task_2() {
    for _ in 1..5 {
        println!("task 2");
        thread::sleep(time::Duration::from_millis(100));
    }
}

async fn task_spawner(){
    let tasks = vec![
        task::spawn(task_1()),
        task::spawn(task_2()),
    ];

    // Aguarda todas as tarefas serem finalizadas antes de finalizar a função 'task_spawmer'.
    join_all(tasks).await;
}

fn main() {
    block_on(task_spawner());
}


