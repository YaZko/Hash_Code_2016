OCB=ocamlbuild -use-ocamlfind -cflag -g

all:
	$(OCB) main.native

out:
	./main.native inputs/logo.in
	./main.native inputs/right_angle.in
	./main.native inputs/learn_and_teach.in

clean:
	rm -rf _build

