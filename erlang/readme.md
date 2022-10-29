
# Erlang

<!-- Language: <a href="readme.md">EN-US</a> -->

<br>

<b>Sumário</b>
- Instalação e Shell
- Estrutura de arquivo
- Programa exemplo (Fatorial)
- Código comentado
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

## Código comentado

<br>

Código comentado com quase todos os recursos da sintaxe do Erlang: https://learnxinyminutes.com/docs/pt-br/erlang-pt/

Para mais códigos com comentários explicativos, veja o diretório <b>tests</b>.

<br>

## Links e Referências

<br>

https://www.erlang.org/doc/getting_started/seq_prog.html

https://www.erlang.org/doc/reference_manual/introduction.html

