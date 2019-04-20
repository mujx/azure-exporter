FROM haskell:8

RUN mkdir -p /app/bin

WORKDIR /app

COPY CHANGELOG.md             /app
COPY LICENSE                  /app
COPY Setup.hs                 /app
COPY azure-exporter.cabal     /app
COPY src/                     /app/src
COPY stack.yaml               /app

RUN stack setup
RUN stack build --ghc-options -j
RUN stack install

RUN cp /root/.local/bin/azure-exporter /app/bin/azure-exporter

WORKDIR /app/bin

EXPOSE 3000

ENTRYPOINT ["/app/bin/azure-exporter"]
