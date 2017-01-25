# prepare the package for release

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docs install check clean

docs:
	R -q -e 'devtools::document()';\
	R -q -e 'devtools::clean_dll()';\

build:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC);\

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz;\
	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz;\

check:
	cd ..;\
	R CMD check --no-build-vignettes --as-cran $(PKGNAME)_$(PKGVERS).tar.gz;\

vignettes:
	R -q -e 'devtools::build_vignettes()';\
	R -q -e 'tools::compactPDF(paths='\''inst/doc'\'', gs_quality='\''ebook'\'')';\

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\

.PHONY: all docs build install check vignettes clean
