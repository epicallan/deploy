# A simple auto deploy service in haskell (early development)

Inspired by now.js and [capatainduck](https://github.com/githubsaturn/captainduckduck/issues)

## Install & deploy with docker

```
docker build -t deploy-hs .

docker run -it -d -p 8080:8080 --name deploy-hs-app deploy-hs

```
