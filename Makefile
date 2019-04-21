build:
	cabal new-build -fdev

run:
	cabal new-run -fdev

up:
	docker-compose up

down:
	docker-compose down

image:
	docker build -t mujx/azure-exporter .

clean:
	rm -rf dist*
