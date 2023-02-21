
# Kind

<br>

- Comandos gerais:

```bash
sudo kubectl get nodes              # retorna nodes
sudo kubectl get pods               # retorna pods
sudo kubectl get rs                 # retorna replicaset
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

Executa pod com container docker:

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

- Deleta pod:

```bash
sudo kubectl delete pod nginx
```

<br>

- Arquivo de configuração ReplicaSet (arquivo: rs.yaml). A ideia dele é poder gerenciar melhor os pods, como garantir sua persistência, mesmo que ele seja deletado.

```yaml
apiVersion: apps/v1 
kind: ReplicaSet
metadata:
    name: nginx 

spec:
    replicas: 3                 # quantidade de replicas simultâneas
                                # para se manter executando.
    selector:
        matchLabels:            # Seletor para selecionar alguma aplicação.
            app: nginx          # Procura nos pods, a aplicação
                                # identificada por "nginx".

    # Template de um pod já incluído na mesma configuração
    # do ReplicaSet, útil para unificar as coisas.
    template:
        metadata:
            labels:
                app: nginx      # Nome da aplicação executada dentro
                                # do pod.
        
        # Configuração exata de um pod.
        spec:
            containers:
                - name: nginx
                  image: nginx
                  ports:
                    - containerPort: 80
```

<br>

Executa ReplicaSet:

```bash
sudo kubectl apply -f rs.yaml
```

<br>


