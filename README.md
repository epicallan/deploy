# A simple auto deploy service in haskell (early development)

[![Build Status](https://travis-ci.org/epicallan/deploy.svg?branch=master)](https://travis-ci.org/epicallan/deploy)


Inspired by now.js and [capatainduck](https://github.com/githubsaturn/captainduckduck/issues)

## Install & deploy with docker

```
docker build -t deploy-hs .

docker run -it -d -p 8888:8888 --name deploy-hs-app deploy-hs

```
