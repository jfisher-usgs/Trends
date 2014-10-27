\name{RunAnalysis}

\alias{RunAnalysis}

\title{Run Trend Analysis}

\description{This function analyses observations for a significant trend.}

\usage{
RunAnalysis(processed.obs, processed.config, path, id, sdate = NA, edate = NA,
            control = survreg.control(iter.max = 100), sig.level = 0.05,
            graphics.type = "", merge.pdfs = TRUE, site.locations = NULL,
            is.seasonality = FALSE, explanatory.var = NULL, is.residual = FALSE,
            thin.obs.mo = NULL)
}

\arguments{
\item{processed.obs}{\code{list}; see documentation for \code{\link{ProcessObs}} function for details.}
\item{processed.config}{\code{data.frame}; see documentation for \code{\link{ProcessConfig}} function for details.}
\item{path}{\code{character}; the path name of the folder where output data is written.}
\item{id}{\code{character}; an analysis identifier that is used to construct output file names.}
\item{sdate, edate}{\code{Date} or \code{character}; the start and end date corresponding to the period of interest.
  The required date format is \code{YYYY-M-D} (\code{\%Y-\%m-\%d}).}
\item{control}{\code{list}; the regression control values in the format produced by the \code{\link{survreg.control}} function.}
\item{sig.level}{\code{numeric}; the significance level to be coupled with the \emph{p}-value, see \sQuote{Value} section.}
\item{graphics.type}{\code{character}; the graphics type for plot figures.
  The default is the \sQuote{active} device, typically the normal screen device.
  A file-based device can be selected by specifying either \code{"\link{pdf}"} or \code{"\link{postscript}"}.}
\item{merge.pdfs}{\code{logical}; if \code{TRUE} and \code{graphics.type = "pdf"} the figures are combined into a single PDF file, see documentation for \code{\link{MergePDFs}} function for details.}
\item{site.locations}{\code{SpatialPointsDataFrame}; the geo-referenced site coordinates with a required \code{data.frame} component of \code{"Site_id"}, a unique site identifier.}
\item{is.seasonality}{\code{logical}; if \code{TRUE}, seasonal patterns are modeled by a trigonometric regression; as covariates in the trend model.}
\item{explanatory.var}{\code{data.frame}; an explanatory variable added to the covariates of the trend model, see value from the \code{\link{ProcessWL}} function for the data table format.
  Note that explanatory variable values are linearly interpolated at sample dates in \code{processed.obs}.}
\item{is.residual}{\code{logical}; if \code{TRUE}, the explanatory variable is transformed using its residuals from linear regression.
  Should be used when the explanatory variable is monotonically increasing or decreasing during the entire trend period.
  Requires specification of the \code{explanatory.var} argument.}
\item{thin.obs.mo}{\code{character}; the full name of a calendar month; if specified, data is thinned to one observation per year collected during this month.
  Allows verification that the variable sampling frequencies don't substantially affect the trend results.
  Thinning the data also could remove serial correlation in the more frequently sampled years.}
}

\details{
The \code{\link{survreg}} function is used to fit a parametric survival regression model to the observed data, both censored and uncensored.
The specific class of survival model is known as the accelerated failure time (AFT) model.
A maximum-likelihood estimation (MLE) method is used to estimate parameters in the AFT model.
The MLE is solved by maximizing the log-likelihood using the Newton-Raphson method, an iterative root-finding algorithm.
The likelihood function is dependent on the distribution of the observed data.
Data is assumed to follow a log-normal distribution because most of the variables have values spanning two or more orders of magnitude.
Note that if all observations are uncensored, the survival regression becomes identical to ordinary least squares regression.
}

\value{
This function returns a \code{data.frame} object with the following components:
\item{Site_id}{\code{numeric}; a unique site identifier.}
\item{Site_name}{\code{character}; a local site name.}
\item{Parameter_id}{\code{character}; a unique parameter identifier.}
\item{Parameter_name}{\code{character}; a common parameter name.}
\item{sdate, edate}{\code{Date}; the start and end date corresponding to the period of interest.}
\item{n}{\code{integer}; the number of observations in the analysis.}
\item{nmissing}{\code{integer}; the number of missing values.}
\item{nexact}{\code{integer}; the number of exact (uncensored) observations.}
\item{nleft}{\code{integer}; the number of left-censored observations.}
\item{ninterval}{\code{integer}; the number of interval-censored observations.}
\item{nbelow.rl}{\code{integer}; the number of observations that are below the reporting level.}
\item{min, max}{\code{numeric}; the minimum and maximum.}
\item{median}{\code{numeric}; the median.}
\item{mean, sd}{\code{numeric}; the mean and standard deviation, set to \code{NA} if censored data is present.}
\item{iter}{\code{numeric}; the number of Newton-Raphson iterations required for convergence. If \code{NA}, the regression failed or ran out of iterations and did not converge.}
\item{slope}{\code{numeric}; the slope of the linear trend over time in percent change per year.}
\item{std.err}{\code{numeric}; the standard error for the linear trend over time in percent change per year.}
\item{p}{\code{numeric}; the \emph{p}-value for linear trend over time.}
\item{p.model}{\code{numeric}; the \emph{p}-value for the parametric survival regression model.}
\item{trend}{\code{character}; significant trends are indicated by a \emph{p}-value (\code{p}) less than or equal to the significance level.
  The sign of the \code{slope} indicates whether the significant trend is positive (\code{"+"}) or negative (\code{"-"}).
  \emph{p}-values greater than the significance level are specified as having no significant trend (\code{"none"}).}
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
