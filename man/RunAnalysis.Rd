\name{RunAnalysis}

\alias{RunAnalysis}

\title{Run Trend Analysis}

\description{This function analyses observations for a significant trend.}

\usage{
RunAnalysis(processed.obs, processed.config, path, id, sdate = NA, edate = NA,
            graphics.type = "", merge.pdfs = TRUE, site.locations = NULL)
}

\arguments{
\item{processed.obs}{\code{list}; see documentation for \code{\link{ProcessObs}} function for details.}
\item{processed.config}{\code{data.frame}; see documentation for \code{\link{ProcessConfig}} function for details.}
\item{path}{\code{character}; the path name of the folder where output data is written.}
\item{id}{\code{character}; an analysis identifier that is used to construct output file names.}
\item{sdate, edate}{\code{Date} or \code{character}; the start and end date corresponding to the period of interest.
  The required date format is \code{YYYY-M-D} (\code{\%Y-\%m-\%d}).}
\item{graphics.type}{\code{character}; the graphics type for plot figures.
  The default is the \sQuote{active} device, typically the normal screen device.
  A file-based device can be selected by specifying either \code{"\link{pdf}"} or \code{"\link{postscript}"}.}
\item{merge.pdfs}{\code{logical}; if \code{TRUE} and \code{graphics.type = "pdf"} the figures are combined into a single PDF file, see documentation for \code{\link{MergePDFs}} function for details.}
\item{site.locations}{\code{\link{SpatialPointsDataFrame}}; the geo-referenced site coordinates with a required \code{data.frame} component of \code{"Site_id"}, a unique site identifier.}
}

\details{
This function uses the \code{\link{survreg}} function to fit a parametric survival regression model, an accelerated failure time (\href{http://en.wikipedia.org/wiki/Accelerated_failure_time_model}{AFT}) model, to the censored data.
The assumed distribution for the censored data is the \code{"lognormal"} distribution.
}

\value{
This function returns a \code{data.frame} object with the following components:
\item{Site_id}{\code{numeric}; a unique site identifier.}
\item{Site_name}{\code{character}; a local site name.}
\item{Parameter_id}{\code{character}; a unique parameter identifier.}
\item{sdate, edate}{\code{Date}; the start and end date corresponding to the period of interest.}
\item{n}{\code{integer}; the number of records in the analysis.}
\item{nmissing}{\code{integer}; the number of missing values.}
\item{nexact}{\code{integer}; the number of exact values.}
\item{nleft}{\code{integer}; the number of left-censored values.}
\item{ninterval}{\code{integer}; the number of interval-censored values.}
\item{nbelow.rl}{\code{integer}; the number of values that are below the recording level.}
\item{min, max}{\code{numeric}; the minimum and maximum values.}
\item{median}{\code{numeric}; the median.}
\item{mean, sd}{\code{numeric}; the mean and standard deviation, set to \code{NA} if censored data is present.}
\item{iter}{\code{numeric}; the number of iterations required for convergence. If \code{NA}, the regression did not converge after 100 iterations.}
\item{c1, c2}{\code{numeric}; the coefficients in the AFT model on the log median survival time scale.}
\item{scale}{\code{numeric}; the reciprocal of the shape parameter.}
\item{p}{\code{numeric}; the \href{http://en.wikipedia.org/wiki/P-value}{p-value} statistic.}
\item{slope}{\code{numeric}; the slope of the linear-predictor in percent change per year.}
\item{trend}{\code{character}; significant trends are indicated by a p-value less than or equal to 0.05, a 5 percent acceptance level.
  The sign of the \code{slope} indicates whether the significant trend is positive (\code{"+"}) or negative (\code{"-"}).
  P-values greater than 0.05 are specified as having no significant trend (\code{"none"}).}
If arguments \code{path} and \code{id} are specified, the returned data table of summary statistics (described above) is written to an external text file.
If in addition a file-based graphics type is selected, plots are drawn to external files.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{DrawPlot}}}

\examples{
# Specify global arguments for reading table formatted data in a text file
read.args <- list(header = TRUE, sep = "\t", colClasses = "character",
                  na.strings = "", fill = TRUE, strip.white = TRUE,
                  comment.char = "", flush = TRUE, stringsAsFactors = FALSE)

# Read input files
path.in <- system.file("extdata", "SIR2014", package = "Trends")
file <- file.path(path.in, "Observations.tsv")
observations <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Parameters.tsv")
parameters <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Detection_Limits.tsv")
detection.limits <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Config_VOC.tsv")
config <- do.call(read.table, c(list(file), read.args))

# Process observations
processed.obs <- ProcessObs(observations, parameters, detection.limits,
                            date.fmt = "\%m/\%d/\%Y")

# Plot data for a single parameter at a specific site
d <- processed.obs[["P32102"]]
d <- d[d$Site_id == "433002113021701", c("Date", "surv")]
DrawPlot(d, main = "RWMC Production", ylab = "Carbon Tetrachloride")

# Configure sites, parameters, and duration for analysis
processed.config <- tail(ProcessConfig(config, processed.obs))

# Run analysis
stats <- RunAnalysis(processed.obs, processed.config,
                     sdate = "1987-01-01", edate = "2012-12-31")
}

\keyword{models}
