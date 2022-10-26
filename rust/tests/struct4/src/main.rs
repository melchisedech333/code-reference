
// Iesus <3

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


