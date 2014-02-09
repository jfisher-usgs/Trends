\name{ReadParameters}

\alias{ReadParameters}

\title{Read Parameters}

\description{
This function reads the symbol type and color for all possible parameters.
}

\usage{ReadParameters(file, draw.legend = FALSE)}

\arguments{
\item{file}{character; file path for the parameter configuration table data,
   see \sQuote{Format}.}
\item{draw.legend}{logical; if \code{TRUE}, parameter points are drawn. This
  should assist with the selection of unique symbols and colors for each
  parameter.}
}

\format{
The imported parameter configuration table requires the following fields:
\code{"Parameter"} (read as row names), \code{"Name"}, \code{"Units"},
\code{"pch"}, \code{"col"}, and \code{"bg"}. Each line of the table represents a
single unique parameter. See \sQuote{Value} for field descriptions.
}

\value{
Returns a data frame with components:
\item{Name}{character; the local parameter name, used for legend text and axis
  labels.}
\item{Units}{character; the parameter units used for axis labels.}
\item{pch}{integer; specifies the symbol to use in plotting points.}
\item{col}{character; the color code.}
\item{bg}{character; the background (fill) color for open plot symbols,
  \code{pch = 21:25.}}
and attribute:
\item{row.names}{character; a unique identifier for each parameter.}
}

\author{J.C. Fisher}

\seealso{\code{\link{read.table}}, \code{\link{legend}}}

\examples{
f <- system.file("extdata/ex.config.parameters.tsv", package = "Trends")
obj <- ReadParameters(f)
str(obj)
}

\keyword{hplot}
