
// IHS s2
// Custom Option<T>

pub enum MyOption<T> {
    NotFoundData,
    FoundData(T)
}

impl<T> MyOption<T> {
    pub const fn is_found(&self) -> bool {
        match self {
            MyOption::NotFoundData => false,
            MyOption::FoundData(_) => true
        }
    }

    pub const fn unwrap_data(&self) -> &T {
        match self {
            MyOption::NotFoundData => {
                panic!("called `MyOption::unwrap_data()` on a `NotFoundData` value");
            },
            MyOption::FoundData(value) => value
        }
    }
}

fn main() {
                             // vec1     vec2
    let data_vectors = vec![ vec![1, 2], vec![1, 2, 3, 4, 5] ];

    // Usando match.
    for index in 0..data_vectors.len() {
        let option = check_data(&data_vectors[index]);
        println!("Vector {index}");

        match option {
            MyOption::NotFoundData => println!("\tData not found!"),
            MyOption::FoundData(value) => println!("\tData found: {value}"),
        }
    }

    println!("\n+++++++++++++++++++++++++++\n");

    // Usando .is_found()
    for index in 0..data_vectors.len() {
        let option = check_data(&data_vectors[index]);
        println!("Vector {index}");

        if option.is_found() {
            println!("\tData found: {}", option.unwrap_data());
        } else {
            println!("\tData not found!");
        }
    }
}

fn check_data(data: &Vec<i32>) -> MyOption<i32> {
    if data.len() < 3 {
        MyOption::NotFoundData
    } else {
        MyOption::FoundData(data[3])
    }
}


