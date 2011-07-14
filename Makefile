
all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c syntax.ml
	ocamlc -c parser.mli parser.ml
	ocamlc -c lexer.ml
	ocamlc -c -I +batteries utils.ml
	ocamlc -c -I +batteries asttree.ml
	ocamlc -c -I +ocamlgraph -I +batteries lgraph.ml
	ocamlc -c -I +ocamlgraph -I +batteries partition.ml
	ocamlc -c -I +batteries -I +ocamlgraph test.ml
		ocamlc -I +ocamlgraph -I +batteries -o test \
		batteries.cma graph.cmo utils.cmo asttree.cmo \
		lexer.cmo  parser.cmo  syntax.cmo  lgraph.cmo partition.cmo test.cmo

clean:
	rm -rf *.cmo *.cmi test *.dot *.ps *.log *.graphml *.fig *.png *.pdf
	rm -rf parser.mli parser.ml lexer.ml
	rm -rf tools/dottoxml/*.pyc
	rm -rf metis*
dot: all
	./test dft.favt -o out.dot
	./prdot.sh out.dot
	python tools/dottoxml/dottoxml.py out.dot out.graphml

#dot -Tps -Grankdir=LR 1.dot > 1.ps
#process the graph a little bit: insert out.log to the 10-th line ... | delete empty lines
ps: dot
	dot -Tps out.dot > out.ps
	evince out.ps
