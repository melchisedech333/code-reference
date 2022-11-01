

// Testbench Code Goes here
module arbiter_tb;

reg clock, reset, req0,req1;
wire gnt0,gnt1;

initial begin
    $monitor ("req0=%b,req1=%b,gnt0=%b,gnt1=%b", req0,req1,gnt0,gnt1);
    
    clock = 0;
    reset = 0;
    req0 = 0;
    req1 = 0;

    #5  reset = 1;
    #15 reset = 0;
    #10 req0 = 1;
    #10 req0 = 0;
    #10 req1 = 1;
    #10 req1 = 0;
    #10 {req0,req1} = 2'b11;
    #10 {req0,req1} = 2'b00;
    #10 $finish;
end

always begin
    #5 clock = !clock;
end

arbiter U0 (
    .clock (clock),
    .reset (reset),
    .req_0 (req0),
    .req_1 (req1),
    .gnt_0 (gnt0),
    .gnt_1 (gnt1)
);

endmodule


