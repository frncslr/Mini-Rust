all: lexer.ml parser.ml type.ml ast.ml env.ml check_singleton_variable.ml substitution.ml unification.ml eval.ml main.ml
	ocamlbuild main.native

clean:
	ocamlbuild -clean
