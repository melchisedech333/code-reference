
// IHS <3

use super::module3::*;

pub fn module2_pub_function() {
    println!("called module2::module2_pub_function()");
}

pub fn module2_ind_function() {
    println!("called module2::module2_ind_function()");

    module2_priv_function();
}

fn module2_priv_function() {
    println!("called module2::module2_priv_function()");

    module3_pub_function();
    module3_ind_function();
}


