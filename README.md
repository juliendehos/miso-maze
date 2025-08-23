# miso-maze

Client-server web app using [Miso](https://github.com/dmjio/miso) and WebSockets:
move inside the maze and meet other players.


TODO


## Build and run:

```
build.sh
cd output
./server
```

then go to `127.0.0.1:3000` (`localhost:3000` won't work)

## Development

- frontend:

```
nix develop .#wasm
make update             # update package repository
make build              # build frontend app
```

- backend:

```
nix develop .#default 
cabal update            # update package repository
cabal test              # build and run tests
cabal run server        # build and run server
```

