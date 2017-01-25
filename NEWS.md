# Trends 1.1.1.9000

- ...

# Trends 1.1.1

- Edit CITATION file

- Update URL links to HTTP Secure

# Trends 1.1.0

- Change to in-source documentation using **roxygen2** package.

# Trends 1.0.0

- Change repository for package installation to https://jfisher-usgs.github.io/R

- Minor changes for compiling with R version 3.3.0.

- Change format for package version numbering from #.#-# to #.#.#

# Trends 0.2-1

- Additional explanatory variables (such as seasonality and water levels) may be added as covariants in the trend model.

# Trends 0.2-0

- This version represents a major change in the package and breaks compatibility with older versions.
  The impetus for this change was a need to model interval-censored data.
  A parametric survival regression model is fit to the observed data, both censored and uncensored.

# Trends 0.1-5

- Fixed bug in `RunTrendAnalysis` function that dropped matrix class for spatial data frames with a single coordinate value.

- Fixed bug in `RunTrendAnalysis` function that tried to compute trend when summary statistics returned from `NADA::cenfit` function were incomplete.

- Added a package CITATION file and a LaTeX bibliography source file.

- Added argument in `MergePDFs` function to remove individual files after merge is completed.

# Trends 0.1-4

- Added a package vignette describing 2014 water quality data processing.

- Summary statistics are written to a shapefile.

- Added new functions for reading configuration files (`ReadParConfig` and `ReadPlotConfig`) and a geo-referenced site location file (`ReadSiteLocations`).

- The 'Min', 'Max', 'Start_date', and 'End_date' fields have been removed from the plot configuration files.
  Users need to update their input files to account for this change.

- Graphics now opened in a platform-independent way using the `grDevices::dev.new` function.

- Many of the function names were updated using a more explicit naming convention.

- The **tcltk** package is no longer required.
  Removed Tcl/Tk dialog boxes for file and directory selection.
  Default directory arguments are the working directory and file path names must be passed as arguments.

- Fixed calculation of the number of samples above the reporting limit (n_above_rl); previous value did not include the number of estimated (E) samples.

- Plot legends show "p-value < 0.001"; previously these labels were shown as "p-value = 0.000".

- Nonparametric regression is prevented when `NADA::cenfit` function results in warnings.

- Warn if site ids in configuration files do not match ids in the data file.

- Added a new function `MergePDFs`, combines PDF files into a new file.
