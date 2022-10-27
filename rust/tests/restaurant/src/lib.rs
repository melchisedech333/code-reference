
// Módulo que contem outros módulos, que contem funções.

// Módulo geral.
mod front_of_house {

    // Módulo 1.
    mod hosting {

        // Funções...
        fn add_to_waitlist() {}
        fn seat_at_table() {}
    }

    // Módulo 2.
    mod serving {

        // Funções...
        fn take_order() {}
        fn serve_order() {}
        fn take_payment() {}
    }
}


