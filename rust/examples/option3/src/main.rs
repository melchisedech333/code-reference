
// IHS s2

fn main() {
    let new_vec = vec![1, 2];
    let bigger_vec = vec![1, 2, 3, 4, 5];
    let vec_of_vecs = vec![new_vec, bigger_vec];

    for vec in vec_of_vecs { // Percorre os vetores.

        // Faz a chamada, retornando um Option<T>.
        let inside_number = take_fifth(vec);

        // Verifica se possui um Some(T).
        // .is_some() retornará um true se houver um Some<T>, se retornar false significa que há um None.
        if inside_number.is_some() {

            // O .unwrap() desembrulha o valor de Some<T> e retorna ele, servindo como um método de 
            // acesso a informação/dado que precisamos, e que está dentro de um Some<T>.
            // O ideal é usar o .unwrap() somente quando se tem certeza que há um Some<T>, pois caso contrário
            // será gerado um erro com panic, e assim parando a aplicação.
            println!("We got: {}", inside_number.unwrap());
        } 
        
        // Verifica se é um None.
        // Pode-se utilizar também .is_none()
        else {
            println!("We got nothing.");
        }
    }
}

// Verifica se os elementos de um vetor da um total menor que 5.
// Caso sim, então retore um None, caso contrário retorna um Some(T).
fn take_fifth(value: Vec<i32>) -> Option<i32> {
    if value.len() < 5 {
        None
    } else {
        Some(value[4])
    }
}


