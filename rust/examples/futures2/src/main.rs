
use futures::executor::block_on;

async fn learn_song() -> String {
    println!("learn_song...");
    String::from("IHS s2")
}

async fn sing_song(_r: String) {
    println!("sing_song...");
}

async fn dance() {
    println!("dance...");
}

async fn learn_and_sing() {
    // Espere até que a música seja aprendida antes de cantá-la. Usamos `.await` 
    // aqui em vez de `block_on` para evitar o bloqueio do thread, o que 
    // torna possível `dançar` ao mesmo tempo.
    let song = learn_song().await;
    sing_song(song).await;
}

async fn async_main() {
    let f1 = learn_and_sing();
    let f2 = dance();

    // `join!` é como `.await` mas pode esperar por vários futuros simultaneamente. 
    // Se estivermos temporariamente bloqueados no futuro `learn_and_sing`, o futuro `dance` 
    // assumirá o controle do tópico atual. Se `dance` for bloqueado, `learn_and_sing` 
    // pode retomar o controle. Se ambos os futuros forem bloqueados, então `async_main` 
    // será bloqueado e cederá ao executor.
    futures::join!(f1, f2);
}

fn main() {
    block_on(async_main());
}


