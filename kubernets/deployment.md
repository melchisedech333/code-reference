
# Kubernets - Deployment

- Cria aplicação em Go.

```bash
go mod init teste
```

Depois cria o arquivo main.go:

```go
package main

import "net/http"

func main () {
	http.HandleFunc("/", hello)
	http.ListenAndServe(":9090", nil)
}

func hello (w http.ResponseWriter, r *http.Request) {
	w.Write([]byte("Iesus Hominum Salvator!"))
}
```

Para testar a aplicação pode digitar: go run main.go <Br>
E acessar http://localhost:9090/

***

<br>

- Cria arquivo Dockerfile, e imagem.

```docker
FROM golang:1.18 as builder

WORKDIR /app
COPY main.go .
COPY go.mod .
RUN GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o server main.go

FROM scratch
COPY --from=builder /app/server .
CMD [ "./server" ]
```

<br>

Gerar imagem docker:

```bash
docker build -t melchisedech333/goserver:latest ./
```

Executar na máquina (para testar).

```bash
docker run --rm -p 9090:9090 melchisedech333/goserver:latest
```

Estando tudo OK, suba a imagem no Docker Hub.

```bash
docker push melchisedech333/goserver:latest
```

A imagem é listada aqui: https://hub.docker.com/u/melchisedech333

<br>

Comando docker de instalação da imagem:
```bash
docker pull melchisedech333/goserver
```


