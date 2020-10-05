MODULES=voting
OBJECTS=$(MODULES:=.cmo)
TEST=voting_test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

zip:
	zip voting.zip *.ml* _tags Makefile .merlin .ocamlinit

clean:
	ocamlbuild -clean
	rm -f voting.zip
