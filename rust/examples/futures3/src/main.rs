
use futures::executor::block_on;
use futures::future::TryFutureExt;
use futures::try_join;

#[derive(Debug)]
struct Book {
    value: String
}

#[derive(Debug)]
struct Music {
    value: String
}

async fn get_book() -> Result<Book, String> { 
    // Ok(Book{value:String::from("book")})
    Err("Book error".to_string()) // Return error.
}

async fn get_music() -> Result<Music, String> {
    Ok(Music{value:String::from("music")})
}

async fn get_book_and_music() {
    let book_fut = get_book().map_err(|error: String| format!("Error: {error}") );
    let music_fut = get_music();

    // Ao utilizar o try_join é possível retornar um Result<T, E>.
    // O que acaba por facilitar no controle das coisas.
    let result : Result<(Book, Music), String> = try_join!(book_fut, music_fut);
    println!("result: {:?}", result);

    // Extrai dados de Result<T, E>.
    let result_data = result.unwrap();
    println!("result: {:?}, {:?}", result_data.0.value, result_data.1.value);
}

fn main() {
    block_on(get_book_and_music());
}


