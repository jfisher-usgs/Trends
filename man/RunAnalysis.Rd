\name{RunAnalysis}

\alias{RunAnalysis}

\title{Run Analysis}

\description{This function analyses observations for a significant trend.}

\usage{
RunAnalysis(processed.obs, processed.config, path, id, sdate = NA, edate = NA,
            graphics.type = "", merge.pdfs = TRUE, site.locations = NULL)
}

\arguments{
\item{processed.obs}{\code{\link{list}}, see \code{\link{ProcessObs}} for details.}
\item{processed.config}{\code{\link{data.frame}}, see \code{\link{ProcessConfig}} for details.}
\item{path}{character, something...}
\item{id}{character, something...}
\item{sdate, edate}{character or \code{\link{Date}}; start and end date corresponding to the period of interest, the required format is \code{\%Y-\%m-\%d} (YYYY-M-D).}
\item{graphics.type}{character; the graphics type for the plots.
  The default is the \sQuote{active} device.
  A file-based device can be selected by specifying either \code{"\link{pdf}"} or \code{"\link{postscript}"}.}
\item{merge.pdfs}{logical; if \code{TRUE} and \code{graphics.type = "pdf"}, plots are combined into a single PDF file, see \code{\link{MergePDFs}} for details.}
\item{site.locations}{\code{\link{SpatialPointsDataFrame}}, something...}
}

\details{
The \code{\link{survreg}} function in R runs parametric accelerated failure time (AFT) models.
}

\value{
This function returns a \code{data.frame} object with the following compoents:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameter}{character; a unique parameter identifier.}
\item{sdate, edate}{Date; start and end date corresponding to the period of interest.}
\item{n}{integer; the number of records in the analysis.}
\item{nmissing}{integer; the number of missing values.}
\item{nexact}{integer; the number of exact values.}
\item{nleft}{integer; the number of left-censored values.}
\item{ninterval}{integer; the number of interval-censored values.}
\item{nbelow.rl}{integer; the number of values that are below the recording level.}
\item{min, max}{numeric; the minimum and maximum values.}
\item{median}{numeric; the median value.}
\item{mean}{numeric; the mean value.}
\item{sd}{numeric; the standard deviation.}
\item{0.95LCL, 0.95UCL}{numeric; the lower and upper 95\% confidence levels.}
\item{c1, c2}{numeric; the coefficents in the AFT model.}
\item{scale}{numeric; the reciprocal of the shape parameter.}
\item{p}{numeric; the p-value statistic.}
\item{slope}{numeric; the slope of the linear-predictor in percent change per year.}
\item{trend}{numeric; something...}
If arguments \code{path} and \code{id} are specified, the table of summary statistics (described above) is written to an external text file.
If in addition, a file-based graphics types is selected, plots are drawn to an external file(s); otherwise, these plots are drawn to the \sQuote{active} device.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{DrawPlot}}}

\examples{
read.args <- list(header = TRUE, sep = "\t", colClasses = "character",
                  na.strings = "", fill = TRUE, strip.white = TRUE,
                  comment.char = "", flush = TRUE, stringsAsFactors = FALSE)
path.in <- system.file("extdata", "SIR2014", package = "Trends")

file <- file.path(path.in, "Observations.tsv")
observations <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Parameters.tsv")
parameters <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Detection_Limits.tsv")
detection.limits <- do.call(read.table, c(list(file), read.args))
processed.obs <- ProcessObs(observations, parameters, detection.limits,
                            date.fmt = "\%m/\%d/\%Y")

d <- processed.obs[["P32102"]]
d <- d[d$Site_id == "433002113021701", c("Date", "surv")]
DrawPlot(d, main = "RWMC Production", ylab = "Carbon Tetrachloride")

file <- file.path(path.in, "Config_VOC.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- tail(ProcessConfig(config, processed.obs))
stats <- RunAnalysis(processed.obs, processed.config,
                     sdate = "1987-01-01", edate = "2012-12-31")
}

\keyword{hplot}
