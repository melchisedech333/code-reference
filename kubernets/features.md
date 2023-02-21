# Kubernets - Features

- Service

Faz load balancer de containers, utilizando o service como uma espécie de interface. Também serve como um mapeador de porta para algum container específico (se estiver executando sozinho no deployment).

File: service.yaml

```yaml
apiVersion: v1
kind: Service
metadata:
    name: nginx-service
spec:
    type: LoadBalancer
    selector:
        app: nginx
    ports:
        - port: 80          # porta do service
          targetPort: 80    # porta do container.
```

***

<br>

- Horizontal Pod Autoscaling

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
    name: nginx-hpa
spec:

    # o HPA vai funcionar em cima do deployment nginx.
    scaleTargetRef:
        apiVersion: apps/v1
        kind: Deployment
        name: nginx

    # Scaling de replicas (range das mesmas).
    minReplicas: 1
    maxReplicas: 10

    # Metricas no qual serão utilizadas para aplicar o autoscaling de replicas.
    metrics:
        - type: Resouce
          resource:
            name: cpu
            target:
                type: Utilization
                averageUtilization: 75
```

***

<br>

- Ingress

```yaml
 apiVersion: networking.k8s.io/v1
 kind: Ingress
 metadata:
   name: nginx-ingress
 spec:
   rules:
   - host: test-ingress.local
     http:
       paths:
        - pathType: Prefix
          path: "/"
          backend:
            service:
                name: nginx-service
                port: 
                    number: 80 # porta do serviço
```


