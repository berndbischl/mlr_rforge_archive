SPHINXBUILD   := sphinx-build
WWWDIR        := ../www

RNWFILES  = $(wildcard *.Rnw)
RNWFILES += $(wildcard tutorial/*.Rnw)
RNWFILES += $(wildcard howto/*.Rnw)
RNWFILES += $(wildcard concept/*.Rnw)
RSTFILES  = $(RNWFILES:.Rnw=.rst)

MLRRDFILES = $(wildcard ../pkg/mlr/man/*.Rd)
MLRHTMLFILES = $(subst ../pkg/mlr/man,_static/rdocs/mlr,$(MLRRDFILES:.Rd=.html))

.PHONY: help clean html linkcheck rdocs Rnw2rst

help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html       to make standalone HTML files"
	@echo "  Rnw2rst    to make the rst files from the Rnw input files"
	@echo "  linkcheck  to check all cross-references / links"

clean:
	@echo "Cleaning up ..." 
	@-rm -f $(MLRHTMLFILES)
	@-rm -fR $(WWWDIR)/* 
	@-rm -f $(RSTFILES) $(RNWFILES:.Rnw=.log) 
	@-rm -f _linkcheck/

html: rdocs Rnw2rst
	$(SPHINXBUILD) -b html . $(WWWDIR)
	@echo
	@echo "Build finished. The HTML pages are in $(WWWDIR)."


linkcheck: rdocs Rnw2rst
	$(SPHINXBUILD) -b linkcheck . _linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in _linkcheck/output.txt."

rdocs: $(MLRHTMLFILES)

Rnw2rst: $(RSTFILES)

%.rst: %.Rnw
	@echo "Building '$@' from '$<' ..."
	@Rscript --vanilla -e "library(\"ascii\"); Sweave(\"$<\", output=\"$@\", driver=RweaveReST)" > /dev/null 2> $(<:.Rnw=.log)

_static/rdocs:
	mkdir $@

_static/rdocs/mlr: _static/rdocs/
	mkdir $@

_static/rdocs/mlr/%.html: ../pkg/mlr/man/%.Rd _static/rdocs/mlr
	R CMD Rdconv -t html -o $@ $<
