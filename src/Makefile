LISPSRC := $(wildcard *.lisp)
MDOBJS := $(patsubst %, doc/obj/%.md, $(LISPSRC)) $(wildcard *.md)

book: doc/cl-6502.pdf doc/cl-6502.epub doc/cl-6502.html

doc/obj/%.lisp.md : %.lisp Makefile
	awk '/^ *[^;]{3}/ {if (last~/^ *;;;/) printf("\n")} {print} {last=$$0}' < $< | sed -E -e 's/^/    /g' -e 's/^    ;;; ?//g' > $@

doc/cl-6502.md: $(MDOBJS) Makefile doc/genbook.sh
	(cd doc; ./genbook.sh) > $@

doc/cl-6502.pdf: doc/cl-6502.md
	pandoc --template=doc/template.latex --latex-engine=pdflatex -V fontsize=10pt -V monofont=droidsansmono -V monoscale=.70 -V verbatimspacing=.85 -V mainfont=droidserif -V sansfont=droidsans -V documentclass:book -V geometry:top=1.0in -V geometry:bottom=0.75in -S --toc --chapters  -o $@ $<

doc/cl-6502.html doc/cl-6502.epub: doc/cl-6502.md
	pandoc -S --toc --chapters -o $@ $<

clean:
	-rm doc/cl-6502.*
	-rm doc/obj/*.md
