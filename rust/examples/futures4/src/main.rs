
use futures::{
    future::FutureExt, // for `.fuse()`
    pin_mut,
    select,
    executor::block_on,
};

async fn task_one() -> i32 {
    for i in 1..100 {
        println!("one: {i}");
    }

    println!("task one!");
    1
}

async fn task_two() -> i32 {
    for i in 1..10 {
        println!("one: {i}");
    }

    println!("task two!");
    2
}

async fn race_tasks() {
    let t1 = task_one().fuse();
    let t2 = task_two().fuse();

    pin_mut!(t1, t2);

    let res_finished = select! {
        v1 = t1 => { println!("task one completed first: {v1}"); v1 },
        v2 = t2 => { println!("task two completed first: {v2}"); v2 },
    };

    println!("Finished value: {res_finished}")
}

fn main() {
    block_on(race_tasks());
}


