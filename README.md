Trends
======

This [R](http://www.r-project.org/ "R") package is for identifying trends in data from multiple observation sites in a monitoring network.
A parametric survival regression model is used to fit the observed data, both censored and uncensored.
The set of standards used for coding **Trends** is documented in [Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml "Google's R Style Guide").

Install
-------

If R is not already installed on your computer, download and install the latest binary distribution from [CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Open an R session and install the required packages using the following commands:

    repos <- c("http://jfisher-usgs.github.com/R", "http://cran.us.r-project.org")
    install.packages("Trends", repos = repos, dependencies = TRUE, type = "both")

Support for merging PDF files into a new file requires [PDFtk Server](http://www.pdflabs.com/tools/pdftk-server/ "pdftk"), a cross-platform command-line tool for working with PDFs; download and install.

Run
---

Load **Trends** in the current R session:

    library(Trends)

See help documentation:

    help(package = "Trends")
