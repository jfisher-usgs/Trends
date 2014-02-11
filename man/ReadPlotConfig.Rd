\name{ReadPlotConfig}

\alias{ReadPlotConfig}

\title{Read Plot Configuration File}

\description{
This functions reads a plot configuration file.
}

\usage{ReadPlotConfig(file, sep = "\t")}

\arguments{
\item{file}{character; path name for the plot configuration file.}
\item{sep}{character; the field separator character.}
}

\format{
The imported plot configuration table requires the following fields:
\code{"Site_id"}, \code{"Site_name"}, \code{"Parameters"}, and
\code{"Axis_title"}. The first line in the data table must contain variable
names. See \sQuote{Value} for field descriptions.
}

\value{
Returns a data frame with components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameters}{character; comma separated unique parameter identifiers.}
\item{Axis_title}{factor; plot label for the y-axis (missing values permitted).}
}

\author{J.C. Fisher}

\seealso{\code{\link{read.table}}}

\examples{
f <- system.file("extdata/ex.config.plots.tsv", package = "Trends")
obj <- ReadPlotConfig(f)
str(obj)
}

\keyword{IO}
