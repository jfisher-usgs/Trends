Trends
======

This [R](http://www.r-project.org/ "R") package is for identifying trends in
data from multiple observation sites in a monitoring network. Nonparametric
regression is applied to both censored and uncensored data.

The set of standards used for coding **Trends** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Install
-------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Install required R packages from CRAN:

    > install.packages("NADA")

Install the **Trends** package:

    > install.packages("devtools")
    > library(devtools)
    > install_github("Trends", username = "jfisher-usgs", ref = "v0.1.3")

Support for merging PDF files into a new file requires
[PDFtk Server](http://www.pdflabs.com/tools/pdftk-server/ "pdftk"),
a cross-platform command-line tool for working with PDFs.

Run
---

Load **Trends** in the current R session:

    > library(Trends)

See examples in help documentation:

    > help(package = "Trends")
