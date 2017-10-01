# Auto deploy service in haskell (very early development)

This service will get built out into a docker container that listens to github and travis webhooks.

It deploys succesful builds on host machines.

Inner working.

This services container's ssh public key is copied onto host machine so that the container can easily deploy containers to the host by way of ssh.
This seems to be the most practical way of interfacing with a host machine while on docker to ochestrate other containers.