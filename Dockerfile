FROM haskell:8

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

COPY . /src

VOLUME ["/var/run/docker.sock"]

RUN stack build

RUN stack install

CMD ["deploy-exe"]