
# Go

## Gerais

```bash
go env                      # Exibe as variáveis de ambiente.
```

```bash
go run main.go              # Executa arquivo go.
go build main.go            # Compila aplicação.
go install                  # Instala em "~/go/bin".
```

```bash
# Instala pacote.
# A mesma é instalada em: ~/go/src/github.com/google/uuid
go get github.com/google/uuid
```

```bash
# Altera o OS de destino.
GOOS=windows go build main.go
```

<br>

***

## Estrutura padão de projeto

```bash
cd ~/go/src/
mkdir -p github.com/melchisedech333/hello-go
cd github.com/melchisedech333/hello-go
```

<br>

Arquivo: <b>main.go</b>

```go
package main

import "fmt"

func main () {
	fmt.Println("IHS s2")
}
```

<br>

```bash
go build main.go            # Compila aplicação.
go install                  # Instala aplicação em ~/go/bin
                            # Na instalação é pego sempre o
                            # nome da pasta, no caso "hello-go".
```

<br>

***

## Variáveis

```go
package main

import "fmt"

// Declaração com tipagem forte.
var a string

func main () {

    // Atribuição de valor.
    a = "IHS"

    // Infere o tipo e atribui o valor.
    // Se infere apenas uma vez, depois usa-se atribuição.
    b := "Salvator"

    fmt.Println(a)
    fmt.Println(b)

    // Tipos variados.
    c := 10
    d := "Iesus"
    e := 3.14
    f := true
    g := `In principio
        erat 
        Verbum
    `

    // Valores.
    fmt.Printf("%v \n", c)
    fmt.Printf("%v \n", d)
    fmt.Printf("%v \n", e)
    fmt.Printf("%v \n", f)
    fmt.Printf("%v \n", g)

    // Tipos.
    fmt.Printf("%T \n", c)
    fmt.Printf("%T \n", d)
    fmt.Printf("%T \n", e)
    fmt.Printf("%T \n", f)
    fmt.Printf("%T \n", g)
}
```

<br>

***

## Funções

```go
package main

import "fmt"

func main() {
	result := soma(10, 20)
	fmt.Printf("%T, %v\n", result, result)
}

func soma(a int, b int) int {
	return a + b
}
```

<br>

***

## Pacotes

Para um exemplo mais elaborado, veja o diretório <b>src/github.com/melchisedech333/pacote</b>.

<br>

Exemplo simples:

```go
// File: main.go

package main

import (
	"fmt"
	"github.com/melchisedech333/pacote/utils"
)

func main() {
	utils.Version()
	fmt.Printf("10 + 10 = %v\n", utils.Sum(10, 20))
}
```

```go
// File: utils/funcs.go

package utils

import "fmt"

// Funções exportáveis.

func Version() {
    fmt.Println("Utils version 1.0")
}

func Sum (a int, b int) int {
    return a + b
}
```

```bash
go run main.go
```

<br>

***





