FROM alpine:3.10.0 as builder

RUN mkdir -p /app/bin

WORKDIR /app

RUN apk upgrade --update-cache --available && \
    apk add ghc \
            alpine-sdk \
            curl \
            gmp \
            gmp-dev \
            libffi \
            libffi-dev \
            musl-dev \
            zlib-dev \
            cabal

COPY CHANGELOG.md             /app
COPY LICENSE                  /app
COPY Setup.hs                 /app
COPY azure-exporter.cabal     /app
COPY src/                     /app/src
COPY stack.yaml               /app

RUN cabal new-update && \
    cabal new-install -fstatic azure-exporter

RUN cp /root/.cabal/bin/azure-exporter /app/bin/azure-exporter

FROM alpine:3.10.0

RUN mkdir -p /app/bin

WORKDIR /app/bin

RUN apk add ca-certificates

COPY --from=builder /app/bin/azure-exporter /app/bin/azure-exporter

EXPOSE 3000

ENTRYPOINT ["/app/bin/azure-exporter"]
