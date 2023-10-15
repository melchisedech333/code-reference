
# Rust

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Hello World
- Cargo
- Arquivos de cabeçalho
- Formatação de strings
- Variáveis e Constantes
- Funções
- Loops
- Condicionais
- Exemplo de código
- Ownership (propriedade)
- References & Borrowing (referências e empréstimos)
- Slice
- Struct
- Enum
- Option\<T\>
- Match
- Biblioteca
- Result\<T\, E\>
- Propagação de erro
- Generic Data Types
- Traits
- Tests
- Async/Await
- Documentação
- Links e Referências

<br>

## Hello World


```rust
fn main() {
    println!("Hello, world!");
}
```

<br>

Para compilar e executar.

```bash
rustc hello_world.rs
./hello_world
```

<br>

## Cargo

<br>

Ao utilizar o comando abaixo, será criado a estrutura básica de um projeto, e o diretório será automaticamente inicializado com git.

<b>Obs:</b> se o cargo for utilizado dentro de um diretório git já previamente inicializado, o novo diretório criado não será inicializado com git.

```bash
cargo new hello_world
```

<br>
Para gerar a estrutura do projeto, sem gerar os arquivos git.

```bash
cargo new hello_world --vcs=none
```

<br>

Diretórios e arquivos criados:

```bash
hello_world/         # Diretório raiz.
    Cargo.toml       # Arquivo de configuração.
    src/main.rs      # Arquivo Rust.
    .gitignore       # Git files.
    .git             # Git files.
```

<br>

Para compilar e executar:

```bash
cd hello_world
cargo build
./target/debug/hello_world
```

Para fazer isso automático:

```bash
cargo run
```

Para realizar uma verificação prévia, para garantir que todo o código está correto para ser compilado:

```bash
cargo check
```

<br>

<b>Dependências:</b> ao inserir uma dependência no arquivo <i>Cargo.toml</i>, basta digitar <i>cargo build</i>, pois elas serão baixadas e instaladas.

Os pacotes do Rust, criados pela comunidade ficam aqui: https://crates.io/

<br>

Para abrir a documentação de dependẽncias basta digitar o comando abaixo. Ele abrirá a documentação localmente no Browser, basta selecionar a de interesse.

```bash
cargo doc --open
```

Para gerar a documentação apenas do projeto, e ignorar todas as dependências.

```bash
cargo doc --no-deps --open
```

<br>

## Arquivos de cabeçalho

<br>

```rust
use std:io;
```

<br>

## Formatação de strings

<br>

Os dois modos abaixo funcionam.

```rust
let a = 5;
let b = 10;

println!("1, a = {a}, b = {b}");
println!("2, a = {}, b = {}", a, b);
```

<br>

## Variáveis e Constantes

<br>

Por padrão as variáveis em Rust são todas imutáveis.

E você pode torná-las mutáveis, bastando adicionar <b>mut</b> na declaração.

<b>Obs: </b> se utiliza o comando <b>let</b> para declarar uma variável.

```rust
let var1 = 10;          // Variável imutável.
let mut var2 = 20;      // Variável mutável.
```

Exemplo de declaração de constantes:

```rust
//    name                    data type   value
const THREE_HOURS_IN_SECONDS: u32       = 60 * 60 * 3;
```

<br>

<b>Sombreamento de variáveis:</b>

```rust
fn main() {

    // Observe que trata-se de uma variável imutável.
    let x = 5;

    // É definida uma nova variável, utilizando o valor da antiga.
    // Essa nova variável possui o mesmo nome da antiga.
    // Neste momento o valor de 'x' será 6.
    let x = x + 1;

    {
        // Aqui repetimos o processo, utilizamos o valor da antiga
        // e realizamos o sombreamento. Esta variável só existe
        // dentro de seu próprio escopo de código.
        let x = x * 2;
        println!("The value of x in the inner scope is: {x}");
    }

    // Ao sair do escopo de código, o sombreamento criado no bloco de
    // código acima é destruído. Fazendo com que a variável volte a 
    // ter o valor 6.
    println!("The value of x is: {x}");
}
```

Saída:
```
The value of x in the inner scope is: 12
The value of x is: 6
```

<br>

Os pontos positivos de utilizar sombreamento é que você está re-declarando uma variável, e devido a isto você pode alterar até seu tipo, veja o exemplo abaixo.

```rust
// Define uma variável do tipo string.
let spaces = "   ";

// Utilizando o sombreamento, define outra variável, mas
// desta vez atribuindo a mesma um dado do tipo numérico.
let spaces = spaces.len();
```

<br>

<b>Declaração de números inteiros:</b>

Você pode usar underline nos números inteiros.

```rust
// Exemplo de número inteiro.
let num1 = 1_333;
let num2 = 1333;

println!("Num1: {num1} - Num2: {num2}"); // Imprime 1333

// Exemplo de número de ponto flutuante.
let num3 = 1.333;

println!("Num3: {num3}"); // Imprime 1.333
```

<br>

<b>Caracteres:</b>

É aceitado UNICODE.

```rust
let c = 'z';
let z: char = 'ℤ'; // tipo explícito.
let heart_eyed_cat = '😻';

println!("Cat: {heart_eyed_cat}"); 
```

<br>

<b>Tuplas:</b>

Uma tupla armazena um grupo de valores de vários tipos, e elas não podem aumentar nem diminuir de tamanho.

```rust
//        tipos            valores
let tup: (i32, f64, u8) = (500, 6.4, 1);
```

Os modos de acessarmos os elementos de uma tupla são os descritos abaixo.

<Br>

Utilizando correspondência de padrões, conhecido como <b>desestruturação</b>:

```rust
// Note que os tipos da tupla foram inferidos.
let tup = (500, 6.4, 1);

// Desestruturação aqui.
let (x, y, z) = tup;

// Acessando elemento 'y'.
println!("The value of y is: {y}");
```

<br>

Acessando através do índice. Isto é feito utilizando um <i>ponto</i>.

```rust
let x: (i32, f64, u8) = (500, 6.4, 1);

println!("1: {}, 2: {}, 3: {}", x.0, x.1, x.2);
```

<br>

<b>Array:</b>

Um array é semelhante a uma tupla, com a diferença que aceita apenas um único tipo de dado para seus elementos.

```rust
// Numérico.
let a = [1, 2, 3, 4, 5];

// Strings.
let months = ["January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"];

// O acesso ocorre como geralmente o é em outras linguagens.
println!("ar1: {}, ar2: {}", a[2], months[8]);
```

<br>

Exemplo de declaração com especificação de tipo e número de elementos:

```rust
let a: [i32; 5] = [1, 2, 3, 4, 5];
```

<br>

Você também pode utilizar expressão semelhante a de cima, para inicializar um array com valores para você.

```rust
// Ambas expressões são equivalentes.
let a = [3; 5]; // Valor; Número de elementos.
let a = [3, 3, 3, 3, 3];
```

<br>

<b>String:</b>

Forma simples e utilizando <b>String::from</b>.

```rust
let str1 = "str 1";
let mut str2 = String::from("str 2");

println!("str1: {str1}");
println!("str2: {str2}");

str2.push_str(" xD"); // append.
println!("str3: {str2}");
```

<br>

## Funções

<br>

<b>Uso básico:</b>

```rust
fn main() {
    println!("Hello, world!");
    another_function();
}

fn another_function() {
    println!("Another function.");
}
```

<br>

<b>Parâmetros:</b>

```rust
fn main() {
    another_function(5);
    print_labeled_measurement(5, 'h');
}

// Único parâmetro.
//                  nome:  tipo
fn another_function(value: i32) {
    println!("Value: {value}");
}

// Múltiplos parâmetros.
fn print_labeled_measurement(value: i32, unit_label: char) {
    println!("The measurement is: {value}{unit_label}");
}
```

<br>

<b>Expressões e instruções:</b>

As expressões retornam valor, as instruções não.

As instruções terminam com ponto-e-vírgula.

```rust
fn main() {

    // Este bloco de código é uma expressão e retorna o valor 4.
    let y = {
        let x = 3;
        x + 1           /* Obs: não vai ponto-e-vírgula aqui. */
    };

    // Imprime 4.
    println!("The value of y is: {y}");
}
```

<br>

<b>Retorno de dados:</b>

O tipo de dado no final do bloco de código de uma função, é o que será retornado para quem chamou a função.

```rust
// name      tipo retornado
//  |         |
fn func() -> i32 {
    333 /* Obs: não vai ponto-e-vírgula aqui. */
}

// Outro exemplo.
fn main() {
    let x = plus_one(5);
    println!("The value of x is: {x}"); // Imprime 6
}

fn plus_one(x: i32) -> i32 {
    x + 1
}
```

É possível utilizar o comando <b>return</b> para retornar dados no meio de uma função.

```rust
fn func() -> i32 {
    return 111;
}
```

<br>

## Condicionais

<br>

<b>Forma básica:</b>

```rust
fn main() {
    let number = 6;

    if number % 4 == 0 {
        println!("number is divisible by 4");
    } else if number % 3 == 0 {
        println!("number is divisible by 3");
    } else if number % 2 == 0 {
        println!("number is divisible by 2");
    } else {
        println!("number is not divisible by 4, 3, or 2");
    }
}
```

<br>

<b>Utilizando <i>if</i> com <i>let</i>:</b>

Obs: isto é possível por que <i>if</i> é uma expressão, e não uma instrução (que terminaria com ponto-e-vírgula).

Só é permitido utilizar o <i>if</i> com <i>let</i> quando o valor de retorno é sempre do mesmo tipo, com tipos diferentes daria erro.

```rust
// Uso correto.
fn main() {
    let condition = true;
    let number = if condition { 5 } else { 6 };

    println!("The value of number is: {number}");
}
```

```rust
// Uso errado.
fn main() {
    let condition = true;
    let number = if condition { 5 } else { "six" }; // Tipos diferentes (ERRADO).

    println!("The value of number is: {number}");
}
```

<br>

## Loops

<br>

Existe os comandos: <b>loop</b>, <b>while</b> e <b>for</b>.

<br>

```rust
// Loop infinito.
fn main() {
    loop {
        println!("again!");
    }
}
```

<br>

<b>Utilizando <i>loop</i> com <i>let</i>.</b>

```rust
fn main() {
    let mut counter = 0;

    let result = loop {
        counter += 1;

        if counter == 10 {

            // Observe que o comando break termina com ponto-e-vírgula.
            // Mas na mesma expressão inserimos o valor de retorno para
            // a nossa 'let'.
            break counter * 2;
        }
    };

    println!("The result is {result}");
}
```

<br>

<b>Rótulos em loops:</b>

O legal desse recurso é que você pode chamar um <b>break</b> passando como parâmetro o rótulo que você definiu para o loop.

Um rótulo é definido utilizando uma aspa simples no início do mesmo.

```rust
fn main() {
    let mut count = 0;
    
    // Define um rótulo para este loop.
    'counting_up: loop {
        println!("count = {count}");
        let mut remaining = 10;

        loop {
            println!("remaining = {remaining}");

            if remaining == 9 {
                break;
            }

            if count == 2 {
                // Da 'break' no loop de acordo com o rótulo.
                println!("Break counting_up loop.");
                break 'counting_up;
            }

            remaining -= 1;
        }

        count += 1;
    }

    println!("End count = {count}");
}
```

<br>

<b>Utilizando while:</b>

```rust
let mut number = 3;

while number != 0 {
    println!("{number}!");

    number -= 1;
}
```

<br>

<b>Utilizando for:</b>

```rust
// Acessa cada um dos elementos do array.
let a = [10, 20, 30, 40, 50];

for element in a {
    println!("the value is: {element}");
}
```

```rust
// Realiza o mesmo do while acima, fazendo a contagem regressiva:
//   3!
//   2!
//   1!
for number in (1..4).rev() {
    println!("{number}!");
}
```

<br>

## Exemplo de código

<br>

Jogo de advinhação.

```rust
/*
** IHS s2
*/

// Importa módulos.
use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    // Gera um número aleatório de 1 a 100.
    let secret_number = rand::thread_rng().gen_range(1..=100);

    loop {
        println!("Enter input.");

        // Define uma variável mutável.
        let mut guess = String::new();
        
        // Realiza leitura da entrada padrão (terminal).
        io::stdin()

            /* Passa como parâmetro a variável.
            * Onde & é uma referência à mesma.
            * E "mut" explicita que ela é mutável. */
            .read_line(&mut guess)
            .expect("Failed to read line.");

        // Converte a string para número inteiro.
        // Obs: foi realizado um "shadow" na variável, podendo reutilizar
        // o mesmo nome de uma variável já anteriormente declarada.
        //
        // Forma utilizando expect.
        // let guess :u32 = guess.trim().parse().expect("Please type a number.");
        //
        // Forma utilizando match, para realizar o tratamento da entrada.
        // Caso obter sucesso, retorna o próprio número.
        // Caso contrário, executa o continue no loop do escopo em questão.
        let guess :u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {guess}");

        // Compara os valores.
        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),

            // Caso o número for igual, finaliza o jogo.
            Ordering::Equal   => {
                println!("You win!");
                break;
            }
        }
    }
}
```

<br>

## Ownership (propriedade)

<br>

<b>copy</b>: vincula o valor 5 a 'x', depois copia o valor de 'x' para 'y'.

```rust
let x = 5;
let y = x;
```

<br>

<b>move: </b>se tratando de <i>dados profundos</i>, Rust copia apenas a referência e não os dados na memória (heap). Ou seja, <b>s1</b> agora é inválido e não poderá mais ser utilizada. Esta situação é denominada <i>move</i>.

```rust
let s1 = String::from("hello");
let s2 = s1;

println!("{}, world!", s2);
```

<br>

<b>clone</b>: para quando você deseja fazer uma cópia dos dados na memória.

```rust
let s1 = String::from("hello");
let s2 = s1.clone();

println!("s1 = {}, s2 = {}", s1, s2);
```

<br>

<b>funções: </b> segue os exemplos comentados.

```rust
/*
 * Exemplo de situação utilizando 'copy'.
 */

fn main() {
    let x = 5;

    // Como se trata de um número inteiro, o próprio dado
    // é simplesmente copiado para dentro da função. 
    makes_copy(x);

    // Como o dado foi copiado, isto continua válido.
    println!("x value: {x}");
}

fn makes_copy(some_integer: i32) {
    println!("copy data: {}", some_integer);
}
```

```rust
/*
 * Exemplo de situação utilizando 'move'.
 */

fn main() {
    let s = String::from("hello");  // s existe dentro do escopo do main

    takes_ownership(s);             // 's' é movida (move) para dentro da função.
                                    // ... e daqui para baixo ela não existe mais.

    // Esta linha de código produz um erro.
    // println!("s value: {s}");

} // Após finalizar o main(), como 's' foi movida, nada especial acontece.

fn takes_ownership(some_string: String) {
    println!("{} world!", some_string);
} // Como agora 's' foi movida para cá, após terminar este 
  // escopo, a memória será liberada.
```

<br>

<b>Transferindo a propriedade:</b>

Quando a função retorna algo, você pode utilizar isso para transferir a propriedade.

```rust
fn main() {

    // Armazena em s1, a string criada dentro da função gives_ownership().
    let s1 = gives_ownership();
    let s2 = String::from("hello");
    
    // Move s2 para dentro da função.
    let s3 = takes_and_gives_back(s2);
    
    println!("s1: {s1}");

    // Esta linha produz um erro.
    // Pois s2 foi movida para dentro da função takes_and_gives_back().
    // println!("s2: {s2}");

    println!("s3: {s3}");

} // A memória de s1 e s3 são liberadas. Já com s2 não é feito nada, pois ele
  // foi movido. 

fn gives_ownership() -> String {
    let some_string = String::from("yours");

    // Retorna a string que foi criada.
    some_string
}

fn takes_and_gives_back(a_string: String) -> String {
    a_string  // move a string para quem chamou a função.
}
```

<br>

## References & Borrowing (referências e empréstimos)

<br>

Para evitar ficar passando a propriedade de uma variável pra lá e pra cá, podemos utilizar referências.

Da mesma maneira que as variáveis, as referências podem ser imutáveis e mutáveis. Uma referência imutável significa que suas informações não podem ser alteradas.

<b>Referência imutável:</b>

```rust
fn main() {

    // s1 existe no escopo do main.
    let s1 = String::from("hello");

    // s1 é passada como referência para a função.
    let len = calculate_length(&s1);

    // s1 existe ainda no escopo do main, pois não foi movida para a função.
    println!("The length of '{}' is {}.", s1, len);
}

// A função aceita uma referência (deste tipo definido no protótipo)
fn calculate_length(s: &String) -> usize {

    // Observe que a realização da leitura dos dados é permitida.
    println!("Str: {s}");

    // Este código produz um erro, pois não é permitido alterar as informações
    // da variável. Pois a referência especificada é imutável.
    // s.push_str(" xD");

    // Observe que ao finalizar o escopo do código, 's' não deixará de existir.
    // Pois a mesma não foi movida, mas apenas passada uma referência.
    s.len()
}
```

<br>

<b>Referência mutável:</b>

```rust
fn main() {
    
    // Declaramos uma string que pertence ao escopo do main.
    let mut s1 = String::from("Iesus");
    println!("s1: {s1}");

    // Passamos uma referência mutável como parâmetro.
    change_data(&mut s1);

    // Exibe valor alterado da variável.
    println!("s1: {s1}");
}

// A função aceita uma referência mutável como parâmetro.
fn change_data (s: &mut String) {

    // O valor da variável é alterado.
    s.push_str(" Salvator");
}
```

<br>

<b>Referências múltiplas:</b>

É possível criar várias referências que apontam para o mesmo valor, deste que as mesmas existam em escopos apropriados.

```rust
let mut s = String::from("hello");

{
    let r1 = &mut s;

} // Ao sair deste escopo, r1 será destruída, logo, não há problemas em
  // fazer novas referências.

// Exemplo de nova referência.
let r2 = &mut s;
```

<br>

É possível ter referências múltiplas dentro do mesmo escopo, desde que você não faça mais uso das referências imutáveis, após você criar uma nova referência mutável.

Esta funcionalidade do Rust é chamada de <i>Non-Lexical Lifetimes</i>. Esta funcionalidade processa o tempo de vida das variáveis.

```rust
let mut s = String::from("hello");

let r1 = &s; // no problem
let r2 = &s; // no problem

// Último uso das referências imutáveis.
println!("{} and {}", r1, r2);
// variables r1 and r2 will not be used after this point

// Criação de nova referência mutável.
let r3 = &mut s; // no problem
println!("{}", r3);
```

<br>

<b>Coisas não permitidas:</b>

Utilizar várias referências mutáveis seguidas, antes de fazer o uso devido delas.

```rust
// Este código da ERRO.
let mut s = String::from("hello");

let r1 = &mut s;
let r2 = &mut s;

println!("{}, {}", r1, r2);
```

<br>

Não é permitido ter uma referência mutável, apontando para outras referências imutáveis (que por sua vez apontam para o mesmo valor).

```rust
// Este código da ERRO.
let mut s = String::from("hello");

let r1 = &s; // no problem
let r2 = &s; // no problem
let r3 = &mut s; // BIG PROBLEM

println!("{}, {}, and {}", r1, r2, r3);
```

<br>

## Slice

<br>

Se tratam de uma referência que você pode criar de uma sequência de elementos de alguma coleção.

Vejamos um exemplo de fatias de strings.

```rust
fn main() {
    let s = String::from("hello world");

    let hello = &s[0..5];
    let world = &s[6..11];

    // Imprime: 'hello' 'world'
    println!("'{}' '{}'", hello, world);
}
```

Algumas noções:

```rust
let s = String::from("hello");

// Equivalem a mesma coisa.
let slice = &s[0..2];
let slice = &s[..2];
```

```rust
let s = String::from("hello");
let len = s.len();

// Equivalem a mesma coisa.
let slice = &s[3..len];
let slice = &s[3..];
```

```rust
let s = String::from("hello");
let len = s.len();

// Equivalem a mesma coisa.
let slice = &s[0..len];
let slice = &s[..];
```

<br>

<b>Retornando a primeira palavra:</b>

<b>Obs:</b> o tipo <i>&str</i> é um tipo para uma fatia de string.

```rust
// O tipo de dado de 's' é do tipo slice, ou seja, uma fatia de string.
let s = "hello world!";
```

```rust
fn main () {
    let s = String::from("hello world");
    let n = first_word(&s);

    println!("first: {}", n);
}

fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
```

O interessante de usar slice, é que quando você tenta realizar alguma ação indevida com os dados, ocorre que o compilador te avisa do erro. Uma vez que slice é uma referência, você não pode usa-la de qualquer jeito, pois elas estão sempre sujeitas as noções das referências do próprio Rust.

<br>

<b>Exemplos dos usos possíveis de slice de string:</b>

```rust
fn main() {
    let my_string = String::from("hello world");

    let word = first_word(&my_string[0..6]);
    let word = first_word(&my_string[..]);

    // Uma referência desta maneira também funciona.
    let word = first_word(&my_string);

    let my_string_literal = "hello world";

    let word = first_word(&my_string_literal[0..6]);
    let word = first_word(&my_string_literal[..]);

    // Também funciona normalmente, pois uma string definida direta é também um slice. 
    let word = first_word(my_string_literal);
}

// Passando um slice de string como parâmetro.
fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}
```

<br>

<b>Outros exemplos de uso do slice:</b>

```rust
fn main() {
    let a = [ 1, 2, 3, 4, 5 ];
    
    // Produz um slice (fatia) dos dados do array.
    let s = &a[1..3];

    // Percorre o novo array (slice).
    // Imprime:
    //    number: 2
    //    number: 3
    
    for number in s {
        println!("number: {}", number);
    }
}
```

<br>

## Struct

<b>Forma básica:</b>

```rust
// Declarando a struct.
struct User {
    active: bool,
    username: String,
    email: String,
    sign_in_count: u64,
}

fn main() {

    // Variável imutável.
    let user1 = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };

    println!("email: {}", user1.email);

    // Variável mutável.
    let mut user2 = User {
        email: String::from("someone@example.com"),
        username: String::from("someusername123"),
        active: true,
        sign_in_count: 1,
    };

    user2.email = String::from("new-mail@mail.com");

    println!("email: {}", user2.email);
}
```

<br>

<b>Atalhos Field Init:</b>

```rust
// Forma comum.
fn build_user(email: String, username: String) -> User {
    User {
        email: email,
        username: username,
        active: true,
        sign_in_count: 1,
    }
}

// Utilizando atalhos Field Init.
fn build_user(email: String, username: String) -> User {
    User {
        email,         // Atalho aqui.
        username,      // Atalho aqui.
        active: true,
        sign_in_count: 1,
    }
}
```

<br>

<b>Tuple Structs:</b>

```rust
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);

fn main() {
    let black = Color(0, 0, 0);
    let origin = Point(0, 0, 0);
}
```

<br>

<b>Imprimindo dados de uma struct (debug):</b>

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    println!("rect1 is {:?}", rect1);
    println!("rect1 is {:#?}", rect1);
}
```

<br>
Utilizando a macro <b>dbg!</b>

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

fn main() {
    let scale = 2;
    let rect1 = Rectangle {
        width: dbg!(30 * scale),
        height: 50,
    };

    dbg!(&rect1);
}
```

<br>

<b>Métodos - Forma básica:</b>

```rust
// Define a struct com alguns tipos de dados.
struct Rectangle {
    width: u32,
    height: u32,
}

// Implementa a struct.
impl Rectangle {

    // Deve-se passar &self ou &mut self no primeiro parâmetro.
    // Pois através desta variável é possível acessar os elementos da instância.
    fn area(&self) -> u32 {
        self.width * self.height
    }

    // Pode-se utilizar métodos com o mesmo nome de atributos (variávels).
    // Com a única diferença que na chamada deverá utilizar () para diferenciar.
    fn width (&self) -> u32 {
        self.width // Note que o acesso ao elemento é feito utilizando ponto.
    }
}

fn main() {

    // Cria uma variável, criando assim uma instância da struct.
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    println!(
        "The area of the rectangle is {} square pixels, width: {}.",
        rect1.area(), // Acessa um método da struct.
        rect1.width()
    );
}
```

<br>

<b>Métodos - Passando outras structs como parâmetro:</b>

```rust
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    // Note que é aceito inclusive o próprio tipo.
    // Passamos aqui apenas uma referência, logo, temos acesso apenas de leitura.
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}

fn main() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    let rect2 = Rectangle {
        width: 10,
        height: 40,
    };

    let rect3 = Rectangle {
        width: 60,
        height: 45,
    };

    // Passando as structs como parâmetro e verificando seus resultados.
    println!("Can rect1 hold rect2? {}", rect1.can_hold(&rect2));
    println!("Can rect1 hold rect3? {}", rect1.can_hold(&rect3));
}
```

<br>

<b>Métodos - Sem o parâmetro self:</b>

```rust
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {

    // Método que retorna uma nova instância.
    fn make_new() -> Self {
        Self {
            width: 10,
            height: 10,
        }
    }

    // Retorna apenas um número.
    fn make_num () -> u32 {
        333
    }
}

fn main() {
    let rect1 = Rectangle::make_new();
    let mut rect2 = Rectangle::make_new();
    let num  = Rectangle::make_num();

    // Modificando a instância.
    rect2.width  = 30;
    rect2.height = 30;

    println!("Rect 1: {}, {}", rect1.width, rect1.height);
    println!("Rect 2: {}, {}", rect2.width, rect2.height);
    println!("Num...: {}", num);
}
```

<br>

<b>Métodos - é permitido separar partes em blocos <i>impl</i> separados:</b>

```rust
impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}
```

<br>

## Enum

<br>

O modo de uso de uma enum é semelhante ao de uma struct, podendo defini-la e utilizar com impl.

```rust
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
```

<br>

## Option\<T\>

<br>

Em linhas gerais, a ideia deste recurso é para suprir a noção do uso do tipo de dado nulo.

<b>Utilizando com o comando match:</b>

```rust
fn main() {
    let name = String::from("naufil");
    
    println!(
        "Character at index 6: {}",

        // Uso direto na match.
        match name.chars().nth(6) {
            Some(c) => c.to_string(),
            None => "No character at index 6!".to_string(),
        }
    )
}
```

<br>

<b>Utilizando como retorno em funções:</b>

```rust
fn main() {
    let five = Some(5);
    let six = plus_one(five);

    if six != None {
        println!("six != None"); // Imprime.
    } else {
        println!("six = None");
    }

    // Passa "None" como parâmetro.
    let none = plus_one(None);

    if none != None {
        println!("none != None");
    } else {
        println!("none = None"); // Imprime.
    }
}

// Função fazendo uso da noção do Option<T>.
fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1), // Retorna o valor incrementando em 1.
    }
}
```

<br>

<b>Alguns casos de uso:</b>

```rust
let a_str: Option<&str> = Some("a str");
let a_string: Option<String> = Some(String::from("a String"));
let a_float: Option<f64> = Some(1.1);
let a_vec: Option<Vec<i32>> = Some(vec![0, 1, 2, 3]);

#[derive(Debug)]
struct Person {
    name: String,
    age: i32,
}

let marie = Person {
    name: String::from("Marie"),
    age: 2,
};

let a_person: Option<Person> = Some(marie);
let maybe_someone: Option<Person> = None;

println!(
    "{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}",
    a_str, a_string, a_float, a_vec, a_person, maybe_someone
);
```

```rust
// Caso muito útil para validações.
let something: Option<&str> = Some("a String"); // Some("a String")
let nothing: Option<&str> = None;   // None

match something {
    Some(text) => println!("We go something: {}", text),
    None => println!("We got nothing."),
}

match nothing {
    Some(something_else) => println!("We go something: {}", something_else),
    None => println!("We got nothing"),
}
```

```rust
// Na passagem de parâmetro para funções.
fn might_print(option: Option<&str>) {
    match option {
        Some(text) => println!("The argument contains the following value: '{}'", text),
        None => println!("The argument contains None."),
    }
}

let something: Option<&str> = Some("some str");
let nothing: Option<&str> = None;

might_print(something);
might_print(nothing);
```

```rust
// Função retornando um Option<T>.
fn contains_char(text: &str, target_c: char) -> Option<&str> {
    if text.chars().any(|ch| ch == target_c) {
        return Some(text);
    } else {
        return None;
    }
}

let a = contains_char("Rust in action", 'a');
let q = contains_char("Rust in action", 'q');

println!("{:?}", a);
println!("{:?}", q);
```

```rust
// Dentro de elementos de struct.

#[derive(Debug)]
struct Person {
    name: String,
    age: Option<i32>,
}

let marie = Person {
    name: String::from("Marie"),
    age: Some(2),
};

let jan = Person {
    name: String::from("Jan"),
    age: None,
};

println!("{:?}\n{:?}", marie, jan);
```

<br>

## Match

Muito útil para condicionais.

```rust
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter,
}

fn value_in_cents(coin: Coin) -> u32 {

    // Verifica o condicional e retorna o valor de acordo com a entrada.
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter => 25,
    }
}

fn main() {
    // Retorna 5.
    println!("Coin: {}", value_in_cents(Coin::Nickel));
}
```

<br>

<b>Semelhante ao if, else:</b>

```rust
fn main() {
    let dice_roll = 9;
    
    match dice_roll {
        3 => println!("number 3"),
        7 => println!("number 7"),
        other => println!("other number: {}", other),
    }
}
```

<br>

<b>Exemplo utilizando Option\<T\></b>

```rust
let config_max = Some(3u8);

match config_max {
    Some(max) => println!("The maximum is configured to be {}", max),
    _ => (),
}
```

<br>

<b>Utilizando if let:</b>

```rust
// Funciona como match.
let config_max = Some(3u8);

if let Some(max) = config_max {
    println!("The maximum is configured to be {}", max);
}
```

<br>

<b>Estes dois códigos equivalem a mesma coisa:</b>

```rust
// Versão com match.
let mut count = 0;
match coin {
    Coin::Quarter(state) => println!("State quarter from {:?}!", state),
    _ => count += 1,
}

// Versão com if let.
let mut count = 0;
if let Coin::Quarter(state) = coin {
    println!("State quarter from {:?}!", state);
} else {
    count += 1;
}
```

<br>

## Biblioteca

<br>

<b>Utilizando o rustc: </b> 

Veja os códigos em <b>lib1_example</b>, <b>lib2_example</b>, <b>lib3_example</b> e <b>lib4_example</b>, no diretório <b>tests</b>.

```rust
// lib.rs

pub fn public_function() {
    println!("called public_function()");
}

pub fn indirect_access() {
    println!("called indirect_access()");
    private_function();
}

fn private_function() {
    println!("called private_function()");
}
```

Compilar:

```bash
rustc --crate-type=lib --crate-name=_lib1 lib.rs
```

Arquivos gerados:

```
lib_lib1.rlib
```

Utilizando em algum código:

```rust
// main.rs

fn main() {
    mylib::public_function();

    // Error! `private_function` is private
    // mylib::private_function();

    mylib::indirect_access();
}
```

Para compilar:

```rust
rustc main.rs --extern mylib=../lib1/lib_lib1.rlib
```

<br>

<b>Utilizando o cargo para criar bibliotecas (crates):</b>

Veja <b>lib5_example</b>, <b>lib6_example</b>.

<br>

No mesmo diretório, digite:

```bash
cargo new library --lib # Para criar a library.
cargo new example       # Para criar um código de exemplo que faz uso da library.
```

<br>

Edite o arquivo <b>lib.rs</b> da <i>library</i> (este é o código da sua biblioteca). Edite o arquivo <b>main.rs</b> do <i>example</i>, este é o código que faz o uso da sua biblioteca.

Para fazer funcionar, deve-se adicionar o parâmetro abaixo no arquivo <b>Cargo.toml</b> do <b>example</b>.

```toml
[dependencies]
library = { path = "../library" }
```

Onde <b>../library</b> é o diretório do <i>crate</i> (library).

<br>

## Result<T, E>

<br>

Funciona basicamente como um Option<T>, mas lida com tratamento de erros voltados mais para ação, e não para o dado/informação em si.

<br>

## Propagação de erro

<br>

Os dois códigos abaixo são equivalentes. Observe que no 2° há um sinal de interrogação `let mut username_file = File::open("hello.txt")?;`

Este sinal de interrogação significa que ao realizar a chamada com o `File::open`, se tudo ocorrer bem será retornado o valor dentro de `Ok(v)`. Ou seja, armazenando o valor em `username_file`. E em seguida simplesmente continuando a execução do código da função.

No entanto, caso a chamada a `File::open` falhar, será retornado para a própria função o equivalente a um `return Err(v)`. Fazendo com que a função `read_username_from_file()` retorne o erro para quem a chamou.

Note que no final da função é retornado `Ok(username)`, pois como tudo foi bem, e como o valor de retorno especificado na assinatura da função é `Result<String, io::Error>`, devemos portanto retornar um tipo `Ok(v)`.

**Obs:** também é possível utilizar este controle com o `Option<T>`.

```rust
use std::fs::File;
use std::io::{self, Read};

fn read_username_from_file() -> Result<String, io::Error> {
    let username_file_result = File::open("hello.txt");

    let mut username_file = match username_file_result {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut username = String::new();

    match username_file.read_to_string(&mut username) {
        Ok(_) => Ok(username),
        Err(e) => Err(e),
    }
}
```


```rust
use std::fs::File;
use std::io::{self, Read};

fn read_username_from_file() -> Result<String, io::Error> {
    let mut username_file = File::open("hello.txt")?;
    let mut username = String::new();
    username_file.read_to_string(&mut username)?;
    Ok(username)
}

```

Outra maneira ainda melhor é realizar a chamada do método `read_to_string(v)` diretamente após o sinal de interrogação. Inclusive sem a necessidade de armazenar o valor de `File::open` em alguma variável. Uma vez que `File::open` retornará a própria instância do mesmo, pois utilizamos o sinal de interrogação (retornando assim um `Ok(v)`), utilizando essa mesma instância retornada podemos já fazer uma chamada para o método `read_to_string(v)`. Onde também fazemos o controle usando o sinal de interrogação para retornar o erro em caso de falha. Se tudo for bem, basta que no final da função retornemos `Ok(username)`.

```rust
use std::fs::File;
use std::io::{self, Read};

fn read_username_from_file() -> Result<String, io::Error> {
    let mut username = String::new();

    File::open("hello.txt")?.read_to_string(&mut username)?;

    Ok(username)
}
```

Também podemos utilizar o controle na função `main`.

```rust
use std::error::Error;
use std::fs::File;

fn main() -> Result<(), Box<dyn Error>> {
    let greeting_file = File::open("hello.txt")?;

    Ok(())
}
```

<br>

## Generic Data Types

<br>

Com os tipos genéricos evitamos a duplicidade de códigos. Segue alguns exemplos.

```rust
fn largest<T>(list: &[T]) -> &T { ...
```

```rust
struct Point<T> {
    x: T,
    y: T,
}

fn main() {
    let integer = Point { x: 5, y: 10 };
    let float = Point { x: 1.0, y: 4.0 };
}
```

```rust
struct Point<T, U> {
    x: T,
    y: U,
}

fn main() {
    let both_integer = Point { x: 5, y: 10 };
    let both_float = Point { x: 1.0, y: 4.0 };
    let integer_and_float = Point { x: 5, y: 4.0 };
}
```

```rust
enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

```rust
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

fn main() {
    let p = Point { x: 5, y: 10 };

    println!("p.x = {}", p.x());
}
```

```rust
struct Point<X1, Y1> {
    x: X1,
    y: Y1,
}

impl<X1, Y1> Point<X1, Y1> {
    fn mixup<X2, Y2>(self, other: Point<X2, Y2>) -> Point<X1, Y2> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

fn main() {
    let p1 = Point { x: 5, y: 10.4 };
    let p2 = Point { x: "Hello", y: 'c' };

    let p3 = p1.mixup(p2);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
}
```

<br>

## Traits

<br>

Uma Trait lembra muito, e acaba que também funcionando, como uma interface ou abstração.

Neste exemplo abaixo definimos uma Trait chamada `Sumary`, e ela define a assinatura de um método, chamado `summarize()`. Em seguida há implementações para essa Trait, sendo elas: NewsArticle e Tweet.

```rust
pub trait Summary {
    fn summarize(&self) -> String;
}
```

```rust
pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {} ({})", self.headline, self.author, self.location)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }
}
```

Utilizando a trait:

```rust
use aggregator::{Summary, Tweet};

fn main() {
    let tweet = Tweet {
        username: String::from("horse_ebooks"),
        content: String::from(
            "of course, as you probably already know, people",
        ),
        reply: false,
        retweet: false,
    };

    println!("1 new tweet: {}", tweet.summarize());
}
```

Uma Trait, além de servir como interface/abstração, pode suportar métodos padrões.

Exemplo: `examples/trait1`.

```rust
pub trait Summary {
    
    // Define método padrão.
    fn summarize(&self) -> String {
        format!("(Read more from {}...)", self.summarize_author()) // Faz chamada a um método
                                                                   // que não é definido como padrão.
    }

    // Define um método, mas não como padrão.
    // Este pode ser re-declarado pelas implementações da Trait.    
    fn summarize_author(&self) -> String;
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

// Define implementação, mas não necessita declarar o método 'summarize'.
// Ainda assim, quando ele for chamado, o método existirá, pois foi definido
// como padrão da definição da própria Trait Summary.
impl Summary for NewsArticle {
    fn summarize_author(&self) -> String {
        format!("@{}", self.author)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    
    // O mesmo para esta implementação, onde apenas o author é modificado/diferente.
    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}
```

<br>

Também é possível passar as Traits como parâmetros para funções, fazendo com que seja aceito como parâmetro qualquer uma das implementações de uma Trait. Ou seja, poderia aceitar como parâmetro `NewsArticle` e `Tweet`, pois ambos implementam a Trait `Summary`. 

Há dois modos de declarar uma Trait como parâmetro em funções.

```rust
// Modo 1.
pub fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

// Modo 2.
pub fn notify<T: Summary>(item: &T) {
    println!("Breaking news! {}", item.summarize());
}
```

Há vantagens nos dois modos, quando temos várias Traits passadas como parâmetro, convêm mais o modo 2. E quando temos uma função mais simples, convêm mais o modo 1.

```rust
// Modo 1.
pub fn notify(item1: &impl Summary, item2: &impl Summary) { ... }

// Modo 2.
pub fn notify<T: Summary>(item1: &T, item2: &T) { ... }
```

<br>

Para aceitar duas implementações Trait de uma vez, podemos utilizar a sintaxe abaixo.

```rust
pub fn notify(item: &(impl Summary + Display)) { ... }

pub fn notify<T: Summary + Display>(item: &T) { ... }
```

<br>

Para tornar as coisas mais claras, também podemos utilizar a cláusula `where`.

Em vez de escrever isso:

```rust
fn some_function<T: Display + Clone, U: Clone + Debug>(t: &T, u: &U) -> i32 { ... }
```

Podemos escrever isso:

```rust
fn some_function<T, U>(t: &T, u: &U) -> i32
where
    T: Display + Clone,
    U: Clone + Debug,
{
    ...
}
```

<br>

Também é possível utilizar uma Trait no retorno de uma função, como no exemplo abaixo. Deste modo é possível retornar sempre alguma implementação da Trait. Neste exemplo, podendo ser `NewsArticle` ou `Tweet`. O legal é que desta maneira evita-se de declarar o tipo concreto.

**Obs:** não é possível retornar na mesma função `NewsArticle` ou `Tweet`, é aceite que se retorne apenas um ou outro. É regra da linguagem e o compilador não aceita.

```rust
fn returns_summarizable() -> impl Summary {
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from(
            "of course, as you probably already know, people",
        ),
        reply: false,
        retweet: false,
    }
}
```

<br>

É possível declarar implementações `Pair` com tipos diferentes para determinadas funções, como no caso da `cmp_display`.

```rust
use std::fmt::Display;

struct Pair<T> {
    x: T,
    y: T,
}

impl<T> Pair<T> {
    fn new(x: T, y: T) -> Self {
        Self { x, y }
    }
}

impl<T: Display + PartialOrd> Pair<T> {
    fn cmp_display(&self) {
        if self.x >= self.y {
            println!("The largest member is x = {}", self.x);
        } else {
            println!("The largest member is y = {}", self.y);
        }
    }
}

fn main() {
    let pair = Pair::new(1, 2);
    pair.cmp_display();
}
```

Mais exemplos:

- `examples/trait3` - Sobrescreve uma implementação padrão.

<br>

## Tests

<br>

Exemplos:

- `examples/test1` - Teste simples.
- `examples/test2` - Exemplo para testar toda uma implementação de uma Trait.
- `examples/test3` - Mesmo que acima, mas separando testes em arquivo.

<br>

## Async/Await

Exemplos:

- `examples/future1` - Simples demonstração do conceito.
- `examples/future2` - Executando várias async functions.
- `examples/future3` - Executa várias async functions, mas com controle de erro usando `Result<T, E>`, e fazendo uso de `TryFutureExt`, e `try_join`.
- `examples/future4` - Exemplo utilizando a macro `select!`.
- `examples/future5` - Exemplo mais **claro** de como funcionam as async functions.
- `examples/future6` - Executando tasks **simultaneas**.
- `examples/future7` - Utilizando **tokio**.
- `examples/future8` - Taferas assíncronas, interação entre ações síncronas e assíncronas, e manipulação de valores de retorno.

<br>

## Documentação

<br>

O melhor ponto para se ter em mente é que é permitido usar Markdown nos comentários Rust.

Exemplos:

- `examples/doc1` - Exemplo de como documentar o código rust.

Lembrando que para gerar a documentação basta executar o comando abaixo.

```bash
cargo doc --no-deps --open
```

Os arquivos ficam salvos em: `./target/doc/doc1/index.html`

Você pode ver uma tabela de como usar o MarkDown suportado pelo Rust aqui: https://commonmark.org/help/

<br>

## Links e Referências

<br>

https://doc.rust-lang.org/stable/book/

https://stackoverflow.com/questions/56504289/why-do-we-use-the-option-enum

http://saidvandeklundert.net/learn/2021-09-01-rust-option-and-result/

[Option and Result](https://dhghomon.github.io/easy_rust/Chapter_31.html)

[Result and errors](https://doc.rust-lang.org/book/ch09-00-error-handling.html)

[Aprenda Rust - YouTube Playlist](https://www.youtube.com/playlist?list=PLjSf4DcGBdiGCNOrCoFgtj0KrUq1MRUME)

[Async Programming Rust](https://rust-lang.github.io/async-book/06_multiple_futures/01_chapter.html)

[Tokio](https://tokio.rs/)

[Rustdoc Book](https://doc.rust-lang.org/rustdoc/how-to-write-documentation.html)


