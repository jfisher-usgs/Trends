\name{RunAnalysis}

\alias{RunAnalysis}

\title{Run Analysis}

\description{This function analyses observations.}

\usage{
RunAnalysis(processed.obs, processed.config, path, id, sdate = NA, edate = NA,
            graphics.type = "", merge.pdfs = TRUE, site.locations = NULL)
}

\arguments{
\item{processed.obs}{something...}
\item{processed.config}{something...}
\item{path}{something...}
\item{id}{something...}
\item{sdate, edate}{character or Date; start and end date corresponding to the
  period of record, in YYYY-M-D (\code{\%Y-\%m-\%d}).}
\item{graphics.type}{character; see \code{\link{pdf}} and
  \code{\link{postscript}} for file-based devices.}
\item{merge.pdfs}{logical; if \code{TRUE} and \code{graphics.type} is
  \code{"pdf"}, plots are combined into a single PDF file, see
  \code{\link{MergePDFs}}.}
\item{site.locations}{something...}
}

\details{
something...
}

\value{
something... For \code{graphics.type} other than \code{"windows"}, this function is used
for the side-effect of drawing plots to external files.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{ProcessObs}}, \code{\link{ProcessConfig}}}

\examples{
read.args <- list(header = TRUE, sep = "\t", colClasses = "character", na.strings = "",
                  fill = TRUE, strip.white = TRUE, comment.char = "", flush = TRUE,
                  stringsAsFactors = FALSE)
path.in <- system.file("extdata", "SIR2014", package = "Trends")

file <- file.path(path.in, "Observations.tsv")
observations <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Parameters.tsv")
parameters <- do.call(read.table, c(list(file), read.args))
file <- file.path(path.in, "Detection_Limits.tsv")
detection.limits <- do.call(read.table, c(list(file), read.args))
processed.obs <- ProcessObs(observations, parameters, detection.limits,
                            date.fmt = "\%m/\%d/\%Y")

file <- file.path(path.in, "Config_VOC.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- tail(ProcessConfig(config, processed.obs))
stats <- RunAnalysis(processed.obs, processed.config,
                     sdate = "1987-01-01", edate = "2012-12-31")
}

\keyword{hplot}
