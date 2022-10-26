
# Rust

Language: <a href="readme.md">EN-US</a>

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

## Links e Referências

<br>

https://doc.rust-lang.org/stable/book/


