
# Go

## Gerais

```bash
go env                      # Exibe as variáveis de ambiente.
```

```bash
go run main.go              # Executa arquivo go.
go build main.go            # Compila aplicação.
```

<br>

***

## Estrutura de projeto

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


