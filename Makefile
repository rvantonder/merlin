all: build ocamlmerlin ocamlmerlin-server

build:
	dune build --always-show-command-line

workspace:
	dune build --always-show-command-line --workspace=dune-workspace.template

ocamlmerlin ocamlmerlin-server:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune build --always-show-command-line --workspace=dune-workspace.test
	dune runtest --workspace=dune-workspace.test

preprocess:
	dune build --always-show-command-line @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
