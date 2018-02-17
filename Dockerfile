FROM haskell:8.2.1

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

COPY . /src

VOLUME ["/var/run/docker.sock"]

RUN stack build

RUN stack install

EXPOSE 8080

CMD ["deploy-exe"]