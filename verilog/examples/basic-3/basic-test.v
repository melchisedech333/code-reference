
/**
 * Iesus Hominum Salvator.
 */

module tests();
    
    reg  inp;
    wire out;

    inverter u0( .inp(inp), .out(out) );

    initial begin
        inp <= 0;

        $monitor("inp: %0b, out: %0b", inp, out);

        #10 inp <= 1;
    end

endmodule


