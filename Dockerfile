FROM haskell:8 as builder

RUN mkdir -p /app/bin

WORKDIR /app

COPY CHANGELOG.md             /app
COPY LICENSE                  /app
COPY Setup.hs                 /app
COPY azure-exporter.cabal     /app
COPY src/                     /app/src
COPY stack.yaml               /app

RUN stack setup
RUN stack build
RUN stack install

RUN cp /root/.local/bin/azure-exporter /app/bin/azure-exporter

FROM debian:9-slim

RUN mkdir -p /app/bin

WORKDIR /app/bin

RUN apt-get update -y && \
    apt-get install -y --no-install-recommends libgmp-dev ca-certificates netbase && \
    apt-get autoremove -y && \
    apt-get autoclean -y

COPY --from=builder /app/bin/azure-exporter /app/bin/azure-exporter

EXPOSE 3000

ENTRYPOINT ["/app/bin/azure-exporter"]
