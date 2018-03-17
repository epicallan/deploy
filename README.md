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

docker run -it -d -p 8888:8888 -v /var/run/docker.sock:/var/run/docker.sock --name deploy-app epicallan/deploy
```

This app requires a config file in the projects being deployed.

An example deploy config file for a simple build in .dhll file format

```

{
      repoName   = [] : Optional Text
    , repoFiles  = [] : Optional (List Text)
    , deployIP   = "http://localhost:8888" : Text
}

```