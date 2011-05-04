SPHINXBUILD   := sphinx-build
WWWDIR        := ../www

RNWFILES  = $(wildcard *.Rnw)
RNWFILES += $(wildcard tutorial/*.Rnw)
RNWFILES += $(wildcard howto/*.Rnw)
RNWFILES += $(wildcard concept/*.Rnw)
RSTFILES  = $(RNWFILES:.Rnw=.rst)

.PHONY: help clean html linkcheck Rnw2rst rdocs

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files"
	@echo "  Rnw2rst    to make the rst files from the Rnw input files"
	@echo "  linkcheck  to check all cross-references / links"
#	@echo "  latexpdf   to make LaTeX files and run them through pdflatex"

clean:
	@echo "Cleaning up ..." 
	@-rm -f depends.mk
	@-rm -fR _static/rdocs/mlr 
	@-rm -fR $(WWWDIR)/* $(RSTFILES) $(RNWFILES:.Rnw=.log) _linkcheck/ 

html: .depends rdocs Rnw2rst
	$(SPHINXBUILD) -b html . $(WWWDIR)
	@echo
	@echo "Build finished. The HTML pages are in $(WWWDIR)."

# latexpdf: Rnw2rst
# 	$(SPHINXBUILD) -b latex . $(BUILDDIR)/latex
# 	@echo "Running LaTeX files through pdflatex..."
# 	make -C $(BUILDDIR)/latex all-pdf
# 	@echo "pdflatex finished; the PDF files are in $(BUILDDIR)/latex."

linkcheck: .depends rdocs Rnw2rst
	$(SPHINXBUILD) -b linkcheck . _linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in _linkcheck/output.txt."

Rnw2rst: $(RSTFILES)
	$(Rscript)

%.rst: %.Rnw
	@echo "Building '$@' from '$<' ..."
	@Rscript --vanilla -e "library(\"ascii\"); ReST(\"$<\", output=\"$@\")" > /dev/null 2> $(<:.Rnw=.log)

_static/rdocs:
	mkdir $@

_static/rdocs/mlr: _static/rdocs/
	mkdir $@

_static/rdocs/mlr/%.html: ../pkg/mlr/man/%.Rd _static/rdocs/mlr

	R CMD Rdconv -t html -o $@ $<

.depends:
	@-rm -f $@
	@for rdfile in ../pkg/mlr/man/*.Rd ; do \
	  htmlfile="_static/rdocs/mlr/$$(basename $$rdfile .Rd).html"; \
	  htmlfiles="$$htmlfiles $$htmlfile"; \
	  echo "$$htmlfile: $$rdfile" >> $@; \
	done; \
	echo "rdocs: $$htmlfiles" >> $@

-include .depends