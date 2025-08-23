# miso-maze

Client-server web app using [Miso](https://github.com/dmjio/miso) and
WebSockets: move inside the maze and meet other players.


## Try online with a local server

- run the websocket server locally:

```
docker run --rm -it -p 9000:9000 juliendehos/miso-maze:latest
```

- in a browser, go to
  [juliendehos.github.io](https://juliendehos.github.io/miso-maze) and use
  `ws://127.0.0.1:9000` for the server url.


## Build and run

- frontend:

```
nix develop .#wasm
make update             # update package repository
make test               # build and run tests
make build              # build frontend app
make serve              # run http server
```

- backend:

```
nix-shell
cabal update            # update package repository
cabal test              # build and run tests
cabal run               # build and run ws server
```

- backend (docker):

```
nix-build docker.nix                                    # build docker image
docker load < result                                    # load image into docker
docker run --rm -it -p 9000:9000 miso-maze:latest       # run docker image
```

