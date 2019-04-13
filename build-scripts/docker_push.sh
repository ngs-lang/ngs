#!/bin/bash 
REPO="$1"
TAG1="$2"
TAG2="$3"

docker login -u $DOCKER_USER -p $DOCKER_PASS
docker build -f Dockerfile -t $REPO:$TAG1 .

[ -z "$TAG2" ] && echo "Skipping second tag" || docker tag $REPO:$TAG1 $REPO:$TAG2

docker push $REPO
