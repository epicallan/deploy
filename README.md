# A simple auto deploy service in haskell (early development)

Inspired by now.js and [capatainduck](https://github.com/githubsaturn/captainduckduck/issues)

# Install

```
docker build -t deploy-hs .
docker run -it -d -p 7777:9090 --name deploy-hs-app deploy-hs
```
