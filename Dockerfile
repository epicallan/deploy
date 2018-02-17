FROM ubuntu:latest

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

COPY . /src

VOLUME ["/var/run/docker.sock"]

RUN wget https://github.com/epicallan/deploy/releases/download/0.1.5/deploy-build.zip

RUN unzip -q deploy-build.zip

WORKDIR /src/deploy-build


EXPOSE 8080

CMD ["deploy-exe"]