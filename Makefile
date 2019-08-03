all: build ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp ocamlmerlin-lsif

build:
	dune build

workspace:
	dune build --workspace=dune-workspace.template merlin.install

ocamlmerlin ocamlmerlin-server ocamlmerlin-lsp ocamlmerlin-lsif:
	ln -s _build/install/default/bin/$@ ./$@

clean:
	dune clean

test:
	dune build --workspace=dune-workspace.test merlin.install
	dune runtest --workspace=dune-workspace.test --force

preprocess:
	dune build @preprocess

promote:
	dune promote

.PHONY: all build dev clean test promote
