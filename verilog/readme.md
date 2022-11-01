
# Verilog

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Consulte os arquivos no diretório: <a href="./examples">examples</a>.</b>

<br>

<b>Sumário</b>
- Hello World
- Portas vetoriais e escalares
- Drivers
- Always
- Assignment
- Funções
- Códigos
- Noções Gerais
- Links e Referências

<br>

## Hello World

<br>

```verilog
/*
 * Simple hello world example.
 * Iesus Hominum Salvator.
 */

module main;

initial
    begin
        $display("Hello, World!");
        $finish;
    end

endmodule
```

Compilar:

```bash
iverilog -o hello hello.v
```

Simulação:

```bash
vvp hello
```

<br>

## Portas vetoriais e escalares

<br>

```verilog
inout read_enable; // port named read_enable is bi-directional
inout [7:0] address; //port "address" is bidirectional (little-endian)
```

<br>

# Drivers

<br>

É por onde os dados podem passar. Só existem dois tipos: fios (wire), e registradores (register).

<br>

```verilog
wire and_gate_output; // "and_gate_output" is a wire that only outputs

reg d_flip_flop_output; // "d_flip_flop_output" is a register; it stores and outputs a value

reg [7:0] address_bus; // "address_bus" is a little-endian 8-bit register
```

<br>

## Always

<br>

```verilog
always  @ (a or b or sel)
begin
  y = 0;
  if (sel == 0) begin
    y = a;
  end else begin
    y = b;
  end
end
```

<br>

## Assignment

<br>

```
 =   blocking assignment (executa o código sequencialmente)
<=   nonblocking assignment (executa em paralelo)
```

Atrasa sua execução em 5 unidades de tempo:

```verilog
always begin
   #5 clk = ~clk;
end
```

```verilog
// Tri-state buffer.
assign out = (enable) ? data : 1'bz;

// Simple buffer.
assign out = data;
```

<br>

## Funções

<br>

```verilog
function parity;
input [31:0] data;
integer i;
begin
  parity = 0;
  for (i= 0; i < 32; i = i + 1) begin
    parity = parity ^ data[i];
  end
end
endfunction
```

<br>

## Noções Gerais

<br>

<b>Números:</b>

```
Sintaxe: <tamanho>'<raiz><valor>;

Exemplo: 4'b0000;
```

Quando \<tamanho\> é menor que \<valor\>, os bits mais à esquerda de \<valor\> são truncados. Quando \<tamanho\> for maior que \<valor\>, os bits mais à esquerda serão preenchidos, com base no valor do bit mais à esquerda em \<valor\>.

- '0' ou '1' mais à esquerda são preenchidos com '0'.
- 'Z' mais à esquerda são preenchidos com 'Z'.
- 'X' mais à esquerda são preenchidos com 'X'.

<b>Nota:</b> X significa desconhecido e Z significa alta impedância, 1 para Logic High ou 1 e 0 para Logic Low ou 0.

<br>

## Códigos

<br>

Diretório: <b>examples</b>.

<br>

- <a href="./examples/hello-world">hello-world</a> - interação com o simulador.

- <a href="./examples/module-1">module-1</a>, <b>counter-1</b> - exemplificando a estrutura geral, envolvendo os wires, regs, estruturas gerais do código, comportamentos, código do dispositivo e código de simulação.

- <a href="./examples/basic-1">basic-1</a> - utilização de transistores N-MOS e P-MOS (nível simplificado).

- <a href="./examples/basic-2">basic-2</a> - utilização do Power Supply e Ground.

- <a href="./examples/basic-3">basic-3</a> - uso dos transistores NMOS, PMOS, especificando detalhes de suas portas, e construindo com eles uma NOT-GATE (CMOS Inverter).

<br>

## Links e Referências

<br>

https://www.asic-world.com/verilog

https://www.chipverify.com/verilog/verilog-tutorial

http://iverilog.icarus.com/

https://hardwarebee.com/

https://www.asic-world.com/verilog/gate1.html

http://www.cs.nthu.edu.tw/~tcwang/4120-spring04/lec5.pdf

https://web.stanford.edu/class/ee183/handouts_win2003/VerilogQuickRef.pdf

https://sutherland-hdl.com/pdfs/verilog_2001_ref_guide.pdf

http://www.referencedesigner.com/tutorials/verilog/verilog_14.php



