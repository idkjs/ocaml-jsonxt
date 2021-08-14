INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build @install

check:
	dune build @check

examples:
	dune build @examples

tests:
	dune runtest

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

doc:
	dune build @doc

clean:
	dune clean

.PHONY: default install uninstall reinstall clean doc
