# Trends

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/Trends.svg?branch=master)](https://travis-ci.org/jfisher-usgs/Trends)

## Overview

The [R](https://www.r-project.org/ "R") package **Trends** is for identifying trends in data from multiple observation sites in a monitoring network.
A parametric survival regression model is used to fit the observed data, both censored and uncensored.

## Install

You can install the stable version of **Trends** from [GitHub](https://jfisher-usgs.github.io/R/) and
its dependencies from [CRAN](https://cran.r-project.org/) using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", getOption("repos"))
install.packages("Trends", repos = repos, dependencies = TRUE)
```

Or use **devtools** to install the development version.

```r
devtools::install_github("USGS-R/Trends")
```

Support for merging PDF files into a new file requires [PDFtk Server](https://www.pdflabs.com/tools/pdftk-server/ "pdftk"),
a cross-platform command-line tool for working with PDFs; download and install.

## Run

Load **Trends** in the current R session

```r
library(Trends)
```

Access package documentation

```r
help(package = "Trends")
```

## Bugs

Please consider reporting bugs and asking questions on the
[Issues page](https://github.com/jfisher-usgs/Trends/issues "Issues page").

## Disclaimer

This software is in the public domain because it contains materials that originally came from the USGS,
an agency of the United States Department of Interior.
For more information, see the
[official USGS copyright policy](https://www2.usgs.gov/visual-id/credit_usgs.html "official USGS copyright policy").

Although this software program has been used by the USGS, no warranty, expressed or implied,
is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
