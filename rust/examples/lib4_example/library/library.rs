
// IHS <3

pub mod module1;
pub mod module2;
pub mod module3;

pub fn library_pub_function() {
    println!("called library::library_pub_function()");
}

pub fn library_ind_function() {
    println!("called library::library_ind_function()");

    library_priv_function();
}

fn library_priv_function() {
    println!("called library::library_priv_function()");

    module1::module1_pub_function();
    module1::module1_ind_function();
}


