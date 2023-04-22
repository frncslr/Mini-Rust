all:
	ocamlbuild -pkgs unix -use-menhir -menhir "menhir --explain --dump" main.native
	mv main.native mini-rust

clean:
	ocamlbuild -clean
	rm -f mini-rust
