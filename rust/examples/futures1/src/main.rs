
use futures::executor::block_on;

async fn hello_world() {
    println!("world");
}

fn main() {

    // Instancia a future mas não a executa.
    let future = hello_world(); // Não imprime nada.

    // Imprime primeiro.
    println!("hello");

    // Executa a future criada.
    // 
    // Note que 'block_on' trava a execução da thread, de modo que
    // se colocar um após o outro, será executado uma coisa por vez.
    block_on(future);
}


