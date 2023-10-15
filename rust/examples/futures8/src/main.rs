
use futures::executor::block_on;
use futures::future::join_all;
use async_std::task;
use std::{thread, time};

async fn task_1() -> Result<i32, ()> {
    let mut total = 0;

    for _ in 1..15 {
        total += 1;
    }

    thread::sleep(time::Duration::from_secs(1));
    println!("Finished 1");
    Ok(total)
}

async fn task_2() -> Result<i32, ()> {
    let mut total = 0;

    for _ in 1..5 {
        total += 1;
    }

    thread::sleep(time::Duration::from_secs(2));
    println!("Finished 2");
    Ok(total)
}

async fn task_spawner() -> Result<i32, ()> {

    // Executa as tarefas de modo assíncrono.
    // let task1 = task::spawn(task_1());
    // let task2 = task::spawn(task_2());
    
    // Aguarda o término de cada uma e captura o resultado retornado.
    // let result1 = task1.await.unwrap();
    // let result2 = task2.await.unwrap();
    // println!("{}, {}", result1, result2);

    // Executa as tarefas de modo assíncrono.
    let tasks = vec![
        task::spawn(task_1()),
        task::spawn(task_2()),
    ];

    println!("Sleeping...");
    thread::sleep(time::Duration::from_secs(3));

    // Aguarda a finalização das tasks e captura seu resultado.
    let results = join_all(tasks).await;
    let mut total = 0;
    
    for item in results {
        match item {
            Ok(value) => {
                println!("value: {value}");
                total += value;
            },
            Err(()) => println!("Error"),
        }
    }

    println!("total: {}", total);

    Ok(total)
}

fn main() {
    let total = block_on(task_spawner()).unwrap();
    println!("total block: {total}");
}


