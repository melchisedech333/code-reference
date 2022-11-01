
/**
 * Iesus Hominum Salvator.
 */

module tests();
    
    wire vdd, gnd;

    supply u0( .vdd(vdd), .gnd(gnd) );

    initial begin
        #10;

        $display("vdd: %0b, gnd: %0b", vdd, gnd);
    end

endmodule


