
// IHS <3

use super::module2::*;

pub fn module1_pub_function() {
    println!("called module1::module1_pub_function()");
}

pub fn module1_ind_function() {
    println!("called module1::module1_ind_function()");

    module1_priv_function();
}

fn module1_priv_function() {
    println!("called module1::module1_priv_function()");

    module2_pub_function();
    module2_ind_function();
}


