
# Kind

<br>

- Comandos gerais:

```bash
sudo kubectl get nodes              # retorna nodes
sudo kubectl get pods               # retorna pods
```

<br>

- Criar cluster:

```bash
sudo kind create cluster --name=esquenta
sudo kubectl cluster-info --context kind-esquenta
```

<br>

- Arquivo de configuração Pod (arquivo: pod.yaml).

```yaml
apiVersion: v1              # Versão da API do kubernets
kind: Pod                   # Recurso que será utilizado no kubernets
metadata:                   # Informações para identificação do pod
    name: nginx             # realização de buscas e coisas semelhantes.
    labels:
        name: nginx

# Especificações
spec:
    containers:
        - name: nginx
          image: nginx
          ports:
            - containerPort: 80
```

<br>

- Executa pod com container docker:

```bash
sudo kubectl apply -f pod.yaml
```

<br>

- Port forward:

Mapeia a porta 80 do container, para a 9000 no host.

```bash
sudo kubectl port-forward pod/nginx 9000:80
```

<br>


