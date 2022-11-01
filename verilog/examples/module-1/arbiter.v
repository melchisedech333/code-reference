
// Iesus Hominum Salvator s2
// https://www.asic-world.com/verilog/verilog_one_day4.html

// Define o módulo, as entradas e saídas.
module arbiter (
    clock,
    reset, // Active high, syn reset
    req_0, // Request 0
    req_1, // Request 1
    gnt_0, // Grant 0  
    gnt_1  // Grant 1  
);

// Define as portas de entrada.
input clock;
input reset;
input req_0;
input req_1;

// Define as portas de saída.
output gnt_0;
output gnt_1;

// Declara os registradores.
reg gnt_0, gnt_1;

// Define o bloco always.
always @ (posedge clock or posedge reset)

if (reset) begin
    gnt_0 <= 0; // Atribuição nonblocking (executa em paralelo).
    gnt_1 <= 0;
end else if (req_0) begin
    gnt_0 <= 1;
    gnt_1 <= 0;
end else if (req_1) begin
    gnt_0 <= 0;
    gnt_1 <= 1;
end

endmodule


