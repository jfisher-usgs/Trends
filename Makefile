# Targets: run using 'make <target>'
#
#   'all'        builds package documentation in 'man' using roxygen2 package
#                builds and checks source package (*.tar.gz)
#                installs package locally
#   'vignettes'  builds package vignettes in 'inst/doc' using knitr package
#   'clean'      removes all files generated by 'make all',
#                even if there were copies there before.
#

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: install docs check

docs:
	R -q -e 'devtools::document()';\
	R -q -e 'devtools::clean_dll()';\

build:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC);\

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz;\
#	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz;\

check:
	cd ..;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz;\
	R CMD build --no-build-vignettes $(PKGSRC);\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz;\
	R CMD check --no-build-vignettes --as-cran $(PKGNAME)_$(PKGVERS).tar.gz;\

vignettes:
	R -q -e 'devtools::build_vignettes()';\
	R -q -e 'tools::compactPDF(paths='\''inst/doc'\'', gs_quality='\''ebook'\'')';\

clean:
	R -q -e 'devtools::clean_dll()';\
	cd ..;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz;\
	$(RM) -r $(PKGNAME).Rcheck/;\

.PHONY: all docs build install check vignettes clean
