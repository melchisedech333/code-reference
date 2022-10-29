
# Erlang

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Instalação e Shell
- Estrutura de arquivo
- Programa exemplo (Fatorial)
- Visualizando detalhes de erro pelo Shell
- Descrição dos códigos
- Links e Referências

<br>

## Instalação e Shell


```bash
sudo apt install erlang -y
```

```bash
erl # open shell
```

<br>

## Estrutura de arquivo

<br>

Arquivo: <b>tut.erl</b>

```erlang
-module(tut).
-export([double/1]).

double(X) ->
    2 * X.
```

Define módulo, o mesmo deve conter o mesmo nome do arquivo (sem a extensão erl):

```erlang
-module(tut).
```

Especifica uma função para ser utilizada por outros módulos. No caso o <b>/1</b> refere-se ao número de argumentos que a função aceita:

```erlang
-export([double/1]).
```

Declara a função:

```erlang
double(X) ->
    2 * X.
```

<br>

<b>Executar o código no shell (erl):</b>

- 1 - abra o shell com o comando <b>erl</b>
- 2 - digite o nome do módulo, seguido da função e o parâmetro:

    ```bash
    tut:double(10).
    ```

<br>

<b>Obs:</b> o ponto no final das expressões indicam que elas chegaram ao fim.

<br>

## Programa exemplo (Fatorial)

<br>

```erlang
-module(fact).
-export([fc/1]).

fc(1) ->
    1;

fc(X) ->
    X * fc(X - 1).
```

<b>Compilar:</b> abra o shell <i>erl</i> e digite o comando abaixo. Onde <b>c</b> significa compile, e <b>fact</b> é o nome do arquivo de código sem a extensão.

```bash
c(fact).
```

Se tudo for bem, a resposta será algo assim:

```bash
{ok,fact}
```

<b>Executar:</b>

```bash
fact:fc(4).
```

<br>

## Visualizando detalhes de erro pelo Shell

<br>

```bash
...
15> atoms:convert(10, test).
** exception error: no function clause matching 
                    atoms:convert(10,test) (atoms.erl, line 5)
16> v(15).                  
{'EXIT',
    {function_clause,
        [{atoms,convert,
             [10,test],
             [{file,"atoms.erl"},{line,5}]},
         {erl_eval,do_apply,6,
             [{file,"erl_eval.erl"},{line,684}]},
         {shell,exprs,7,
             [{file,"shell.erl"},{line,686}]},
         {shell,eval_exprs,7,
             [{file,"shell.erl"},{line,642}]},
         {shell,eval_loop,3,
             [{file,"shell.erl"},{line,627}]}]}}
17> 
```

<br>

## Descrição dos códigos

<br>

Segue a descrição dos códigos para consultas.

- <a href="tests/fact.erl">fact.erl</a> - código de fatorial mostrando noções como a ideia de declarar apenas parte do código da função (utilizando ponto e vírgula). Também mostra noções como exportar funções.

- <a href="tests/mult.erl">mult.erl</a> - código mostrando como exportar funções com mais de um argumento.

- <a href="tests/fact_mult.erl">fact_mult.erl</a> - código mostrando como declarar várias funções e exportá-las, bem como utilizar mais de um argumento nas funções.

- <a href="tests/atoms.erl">atoms.erl</a> - exemplo de utilização de átomos como argumentos em variáveis.

- <a href="tests/tuples.erl">tuples.erl</a> - tuplas nos parâmetros das funções e nos retornos.

- <a href="tests/list1.erl">list1.erl</a> - processa lista, retornando o tamanho dela.

- <a href="tests/color.erl">color.erl</a> - exemplo de uso dos maps.

- <a href="tests/iotest.erl">iotest.erl</a> - utilizando a saída padrão.

- <a href="tests/convert_celsius.erl">convert_celsius.erl</a> - converte uma lista de temperaturas para celsius.

- <a href="tests/list_max.erl">list_max.erl</a> - encontra o maior valor de uma lista.

- <a href="tests/list_min.erl">list_min.erl</a> - encontra o menor valor de uma lista.

- <a href="tests/invert.erl">invert.erl</a> - inverte a ordem dos elementos de uma lista.

- <a href="tests/tempe.erl">tempe.erl</a> - encontra a temperatura mais alta e faz uso dos condicionais.

- <a href="tests/cond1.erl">cond1.erl</a> - demonstra o uso do if.

- <a href="tests/case1.erl">case1.erl</a> - demonstra o uso do case.

- <a href="tests/year_prog.erl">year_prog.erl</a> - faz uso de if atribuindo um valor a uma variável, e faz o uso do <b>when</b> dentro do <b>case</b>.

- <a href="tests/tests.erl">tests.erl</a> - testando o uso de variáveis.

- <a href="tests/funcs.erl">funcs.erl</a> - utilizando funções de nível superior (funções anônimas).

- <a href="tests/list_funcs.erl">list_funcs.erl</a> - funções boas para trabalhar com listas (foreach, map). Elas acessam e manipulam os elementos de uma lista. 

- <a href="tests/learnerlang-pt.erl">learnerlang-pt.erl</a> - código com várias dicas da sintaxe.

<br>

## Links e Referências

<br>

https://www.erlang.org/doc/getting_started/seq_prog.html

https://www.erlang.org/doc/reference_manual/introduction.html

https://learnxinyminutes.com/docs/pt-br/erlang-pt/


