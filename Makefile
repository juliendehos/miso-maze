
.PHONY= update build optim test todo

all: clean update test build optim

update:
	wasm32-wasi-cabal update

todo:
	find src -name "*.hs" | xargs grep -i todo

test:
	wasm32-wasi-cabal build test
	$(eval my_spec=$(shell wasm32-wasi-cabal list-bin test | tail -n 1))
	wasmi_cli $(my_spec)

build:
	wasm32-wasi-cabal build app
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public output

