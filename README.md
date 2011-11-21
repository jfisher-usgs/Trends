Trends: Analysis of Data Collected from a Monitoring Network
============================================================

Description
-----------

This [R](http://www.r-project.org/ "R") package
is for identifying trends in data from multiple observation sites.
Nonparametric regression is applied to both censored and uncensored data.

The set of standards used for coding **RNWIS** is documented in
[Google's R Style Guide](http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html "Google's R Style Guide").

Installation
------------

If R is not already installed on your
computer, download and install the latest binary distribution from
[CRAN](http://cran.r-project.org/ "The Comprehensive R Archive Network").
Windows users should set R to operate as an SDI application during installation
by choosing to customize the startup options and specifying the SDI interface
(not the default).

Install required R packages from CRAN using a simple call to
`install.packages()`:

    > install.packages(c('NADA'))

Install the **Trends** package:

    > install.packages('Trends', repos='ftp://ftpint.usgs.gov/private/wr/id/scoville/Fisher/Trends')

Running
-------

Load **Trends** in the current R session:

    > library(Trends)

An example workflow is provided in `inst/Readme.txt`.

Updating
--------

Install **RNWIS** package updates:

    > update.packages(repos='ftp://ftpint.usgs.gov/private/wr/id/scoville/Fisher/Trends')
