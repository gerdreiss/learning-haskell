#!/bin/bash

docker build -t mine/ihaskell .

docker run -d \
    --rm -p 8888:8888 \
    --volume `pwd`/work:/home/jovyan/work \
    --env JUPYTER_ENABLE_LAB=yes \
    --env JUPYTER_TOKEN=x \
    --name ihaskell mine/ihaskell:latest
