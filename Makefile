# prepare the package for release

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docs install check clean

docs:
	R -q -e 'pkgload::load_all()';\
	R -q -e 'roxygen2::roxygenize()';\
	R -q -e 'pkgbuild::clean_dll()';\

build:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC);\

install: build
	cd ..;\
	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz;\

check:
	cd ..;\
	R CMD check --no-build-vignettes --as-cran $(PKGNAME)_$(PKGVERS).tar.gz;\

vignettes:
	R -q -e 'inlmisc::BuildVignettes(gs_quality='\''ebook'\'')';\

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\

.PHONY: all docs build install check vignettes clean
