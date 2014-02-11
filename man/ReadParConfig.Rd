\name{ReadParConfig}

\alias{ReadParConfig}

\title{Read Parameter Configuration File}

\description{
This function reads the symbol type and color for all possible parameters.
}

\usage{ReadParConfig(file, sep = "\t", draw.legend = FALSE)}

\arguments{
\item{file}{character; file path for the parameter configuration table data.}
\item{sep}{character; the field separator character.}
\item{draw.legend}{logical; if \code{TRUE}, parameter points are drawn. This
  should assist with the selection of unique symbols and colors for each
  parameter.}
}

\format{
The imported parameter configuration table requires the following fields:
\code{"Parameter"} (read as row names), \code{"Name"}, \code{"Units"},
\code{"pch"}, \code{"col"}, and \code{"bg"}. Each line of the table represents a
single unique parameter. The first line in the table must contain variable
names. See \sQuote{Value} for field descriptions.
}

\value{
Returns a data frame with components:
\item{Name}{character; the local parameter name, used for legend text and axis
  labels.}
\item{Units}{factor; the parameter units used for axis labels.}
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
obj <- ReadParConfig(f, draw.legend = TRUE)
str(obj)
}

\keyword{IO}
