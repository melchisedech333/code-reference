
/**
 * Iesus Hominum Salvator.
 */

module inverter ( input inp, output out );

    supply1 vdd;
    supply0 gnd;

    //
    // Transistor: P, N.
    //   -> output, input, control
    //
    
    //   drain   source   gate
    pmos(out,    vdd,     inp);

    //   source  drain    gate
    nmos(out,    gnd,     inp);

endmodule


