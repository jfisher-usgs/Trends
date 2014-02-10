\name{ReadPlotConfig}

\alias{ReadPlotConfigObs}
\alias{ReadPlotConfigTrends}

\title{Read Plot Configuration File}

\description{
These functions read configurations for observed and trend data plots.
}

\usage{
ReadPlotConfigObs(file, sep = "\t")

ReadPlotConfigTrends(file, sep = "\t")
}

\arguments{
\item{file}{character; file path for the plot configuration table data,
   see \sQuote{Format}.}
\item{sep}{character; the field separator character.}
}

\format{
For \code{ReadPlotConfigObs}, the imported plot configuration table requires the
following fields:
\code{"Site_id"}, \code{"Site_name"}, \code{"Parameters"}, \code{"Min"},
\code{"Max"}, and \code{"Axis_title"}.
For \code{ReadPlotConfigTrends}, the required fields are:
\code{"Site_id"}, \code{"Site_name"}, \code{"Parameters"}, \code{"Start_date"},
and \code{"End_date"}. The required date format is M/D/YYYY
(\code{\%m/\%d/\%Y}). Each line of the table represents an observation site.
The first line in the table must contain variable names. See \sQuote{Value} for
field descriptions.
}

\value{
For \code{ReadPlotConfigObs}, returns a data frame with components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameters}{character; comma separated unique parameter identifiers.}
\item{Min}{numeric; minimum plot limit on the y-axis.}
\item{Max}{numeric; maximum plot limit on the y-axis.}
\item{Axis_title}{factor; plot label for the y-axis.}
For \code{ReadPlotConfigTrends}, returns a data frame with components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameters}{character; comma separated unique parameter identifiers.}
\item{Start_date}{POSIXct; minimum date range for records (x-axis limit).}
\item{End_date}{POSIXct; maximum date range for records (x-axis limit).}
}

\author{J.C. Fisher}

\seealso{\code{\link{read.table}}}

\examples{
f <- system.file("extdata/ex.config.plots.tsv", package = "Trends")
obj <- ReadPlotConfigObs(f)
str(obj)

f <- system.file("extdata/ex.config.censored.tsv", package = "Trends")
obj <- ReadPlotConfigTrends(f)
str(obj)
}

\keyword{IO}
