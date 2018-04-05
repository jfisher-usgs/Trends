# Trends

[![Travis-CI Build Status](https://travis-ci.org/jfisher-usgs/Trends.svg?branch=master)](https://travis-ci.org/jfisher-usgs/Trends)

## Overview

The [R](https://www.r-project.org/ "R") package **Trends** is for identifying trends in data from multiple observation sites in a monitoring network.
A parametric survival regression model is used to fit the observed data, both censored and uncensored.

## Install

You can install the stable version of **Trends** from [GitHub](https://jfisher-usgs.github.io/R/) and
its dependencies from [CRAN](https://cran.r-project.org/) using the following commands:

```r
repos <- c("https://jfisher-usgs.github.io/R", "https://cloud.r-project.org/")
install.packages("Trends", repos = repos, dependencies = TRUE)
```

Or use **devtools** to install the development version.

```r
devtools::install_github("jfisher-usgs/Trends")
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

## Contact

Please consider reporting bugs and asking questions on the
[Issues page](https://github.com/jfisher-usgs/Trends/issues "Issues page").

## Disclaimer

This information is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science.
The information has not received final approval by the U.S. Geological Survey (USGS)
and is provided on the condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

Although this software program has been used by the U.S. Geological Survey (USGS),
no warranty, expressed or implied, is made by the USGS or the U.S. Government
as to the accuracy and functioning of the program and related program material
nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the USGS in connection therewith.

