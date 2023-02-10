
# Docker

<b>Table of contents</b>

- Commands
- Dockerfile
- Multi Container Environments
- Network
- Reference

<br>

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
docker container stop NAME
docker container rm NAME

docker search IMAGE-NAME
docker search elasticsearch

docker container ls                     # List containers
docker container logs CONTAINER-NAME    # View logs

docker network ls                       # List networks
docker network inspect bridge           # Show network information
```

```bash
# -> Pull image and run:

# Get image from Hub Docker. 
docker pull IMAGE-NAME
docker pull busybox

# Get image of specific version.
docker pull ubuntu:18.04

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

<br>

## Dockerfile

Syntax:

```docker
# Comment
INSTRUCTION arguments
```

```docker
# Comment
RUN echo 'IHS s2'
```

<br>

Example:

```bash
git clone https://github.com/prakhar1989/docker-curriculum.git
cd docker-curriculum/flask-app
```

Filename: Dockerfile
```docker
# image base
FROM python:3.8

# set a directory for the app
WORKDIR /usr/src/app

# copy all the files to the container
COPY . .

# install dependencies
RUN pip install --no-cache-dir -r requirements.txt

# define the port number the container should expose
EXPOSE 5000

# Run app / run command
CMD ["python", "./app.py"]
```

<br>

Create image from Dockerfile:

```bash
# Make image
docker build -t melchisedech333/catnip .

# List images
docker images

# Run image
docker run -p 8181:5000 melchisedech333/catnip

# ... acess http://localhost:8181
```

<br>

<b>Publish on Docker Hub:</b>

```bash
docker login
```

Output:
```
Login with your Docker ID to push and pull images from Docker Hub. If you don't have a Docker ID, head over to https://hub.docker.com to create one.
Username: abcd
Password: 
WARNING! Your password will be stored unencrypted in /home/user/.docker/config.json.
Configure a credential helper to remove this warning. See
https://docs.docker.com/engine/reference/commandline/login/#credentials-store

Login Succeeded
```

Publish:

```bash
docker push melchisedech333/catnip
```

Repos: https://hub.docker.com/u/USERNAME-HERE

Repos: https://hub.docker.com/u/melchisedech333

<br>

## Multi Container Environments

```bash
# Project repo
git clone https://github.com/prakhar1989/FoodTrucks
cd FoodTrucks

# Install elasticsearch image
docker pull docker.elastic.co/elasticsearch/elasticsearch:6.3.2

# Run elasticsearch
#   --name = new name
#   -e "discovery.type... = environment variable, run as a single-node.

# Run normal
# docker run -d --name es -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:6.3.2

# Run in network
docker run -d --name es --net foodtrucks-net -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:6.3.2

# Test elasticsearch
curl 0.0.0.0:9200
# Output:
# {
#   "name" : "EN3bcr6",
#   "cluster_name" : "docker-cluster",
#   "cluster_uuid" : "_MjrjI9GSXefvxrSHfcMrg",
#   "version" : {
#     "number" : "6.3.2",
#     "build_flavor" : "default",
#     "build_type" : "tar",
#     "build_hash" : "053779d",
#     "build_date" : "2018-07-20T05:20:23.451332Z",
#     "build_snapshot" : false,
#     "lucene_version" : "7.3.1",
#     "minimum_wire_compatibility_version" : "5.6.0",
#     "minimum_index_compatibility_version" : "5.0.0"
#   },
#   "tagline" : "You Know, for Search"
# }

# Create docker image.
docker build -t melchisedech333/foodtrucks-web .

# Test elasticseach connection:
docker run -it --rm --network foodtrucks-net melchisedech333/foodtrucks-web bash

curl es:9200 # elastic response output...

# Run app...
docker run -P --rm melchisedech333/foodtrucks-web

# In network
docker run -d --net foodtrucks-net -p 5000:5000 --name foodtrucks-web melchisedech333/foodtrucks-web
```

<br>
Script:

```bash
#!/bin/bash

# build the flask container
docker build -t yourusername/foodtrucks-web .

# create the network
docker network create foodtrucks-net

# start the ES container
docker run -d --name es --net foodtrucks-net -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:6.3.2

# start the flask app container
docker run -d --net foodtrucks-net -p 5000:5000 --name foodtrucks-web yourusername/foodtrucks-web
```

<br>

## Network

List networks:
```bash
docket network ls
```

Create new network:
```bash
docker network create NAME
docker network create foodtrucks-net
```

... Run a container:

```bash
# --net foodtrucks-net
docker run -d --name es --net foodtrucks-net -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" docker.elastic.co/elasticsearch/elasticsearch:6.3.2
```

Check network:

```bash
docker network inspect foodtrucks-net
```

<br>

## Reference

- [https://docker-curriculum.com/](https://docker-curriculum.com/)


