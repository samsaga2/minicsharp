all:
	ocamlbuild -use-menhir -menhir "menhir -v" minicsharp.native

clean:
	@rm -rf _build *.cmi *.cmo lexer.ml minicsharp.native*
