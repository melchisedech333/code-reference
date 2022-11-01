
/**
 * Iesus Hominum Salvator.
 * Gate control example.
 */

module transistors ( input  pin_gate, pin_vdd,
                  output pin_p_out, pin_n_out );

    // drain, source, gate
    pmos(pin_p_out, pin_vdd, pin_gate);
    nmos(pin_n_out, pin_vdd, pin_gate);

endmodule


