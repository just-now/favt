OINC=-I +batteries -I +ocamlgraph -I +camomile -I +cairo
all:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c syntax.ml
	ocamlc -c  parser.mli parser.ml
	ocamlc -c  lexer.ml
	ocamlc -c  $(OINC) utils.ml
	ocamlc -c  $(OINC) asttree.ml
	ocamlc -c  $(OINC) lgraph.ml
	ocamlc -c  $(OINC) partition.ml
	ocamlc -c  $(OINC) place.ml
	ocamlc -c  $(OINC) test.ml
	ocamlc $(OINC) -o test \
		unix.cma nums.cma bigarray.cma camomile.cma batteries.cma cairo.cma \
		graph.cmo utils.cmo asttree.cmo \
		lexer.cmo  parser.cmo  syntax.cmo  lgraph.cmo \
		partition.cmo place.cmo test.cmo

clean:
	rm -rf *.cmo *.cmi test *.dot *.ps *.log *.graphml *.fig *.png *.pdf *.cmx *.o
	rm -rf parser.mli parser.ml lexer.ml
	rm -rf tools/dottoxml/*.pyc
	rm -rf metis*
dot: all
	./test dft.favt -o out.dot
	./prdot.sh out.dot
	python tools/dottoxml/dottoxml.py out.dot out.graphml
	./prdot.sh out2.dot
	python tools/dottoxml/dottoxml.py out2.dot out2.graphml

#dot -Tps -Grankdir=LR 1.dot > 1.ps
#process the graph a little bit: insert out.log to the 10-th line ... | delete empty lines
ps: dot
	dot -Tps out.dot > out.ps
	evince out.ps
