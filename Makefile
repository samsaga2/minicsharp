all:
	ocamlbuild -use-menhir -menhir "menhir -v" -use-ocamlfind minicsharp.native

clean:
	ocamlbuild -clean
