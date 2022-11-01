
module tests();
    
    reg p_gate, p_vdd;
    wire p_out, n_out;

    transistors u0( 
        .pin_gate(p_gate), .pin_vdd(p_vdd), 
        .pin_p_out(p_out), .pin_n_out(n_out) );

    initial begin
        p_gate <= 0;
        p_vdd  <= 0;

        $monitor("vdd: %0b, gate: %0b, p-out: %0b, n-out: %0b", 
            p_vdd, p_gate, p_out, n_out);

        #10 p_vdd  <= 1;
        #10 p_gate <= 1;
        #10 p_vdd  <= 0;
    end

endmodule


