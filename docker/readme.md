
# Docker

## Commands

```bash
# -> Check docker is OK:
sudo docker run hello-world
```

```bash
# -> General commands:

docker --help                           # H...
docker run --help

docker ps                               # list containers
docker ps -a

docker images                           # list images
docker run -it IMAGE-REPO-NAME          # run image

docker stop CONTAINER-NAME              # ...
docker kill CONTAINER-NAME              # ...
```

```bash
# -> Pull image and run:

# Get image from Hub Docker. 
docker pull IMAGE-NAME
docker pull busybox

# Run...
docker run -it busybox # Interactive tty
docker run busybox echo "IHS"

# Run custom:
#   -d = detach terminal
#   -P = publish all exposed ports to random ports
#   --name = new name to container
docker run -d -P --name static-site prakhar1989/static-site
# ...
# Show ports:
docker port static-site
# ...
# Custom ports:
docker run -p 8888:80 prakhar1989/static-site

```

```bash
# -> Delete containers:

# Lista all.
docker ps -a

# Delete by ID.
docker rm CONTAINER_ID
docker rm 4fa446aa2f3f ce88c1f3aec8

# Delete all.
#   -q = get IDs
#   -f = filter output
docker rm $(docker ps -a -q -f status=exited)
```

```bash
# -> 


```


