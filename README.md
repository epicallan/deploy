# A simple auto deploy service in haskell

[![Build Status](https://travis-ci.org/epicallan/deploy.svg?branch=master)](https://travis-ci.org/epicallan/deploy)


Inspired by now.js and [capatainduck](https://github.com/githubsaturn/captainduckduck/issues)

## Install
-------------

from source

```
docker build -t deploy .

docker run -it -d -P -v /var/run/docker.sock:/var/run/docker.sock --name deploy-app deploy
```

from dockhub

```

docker run -it -d -P -v /var/run/docker.sock:/var/run/docker.sock --name deploy-app epicallan/deploy
```