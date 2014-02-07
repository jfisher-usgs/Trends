\name{ShowParameters}

\alias{ShowParameters}

\title{Draw All Parameters in a Legend}

\description{
This function draws all possible parameters within a legend, and
should assist with the selection of unique symbols and colors for
each parameter.
}

\usage{ShowParameters(file.par = NULL)}

\arguments{
\item{file.par}{character; file path for the parameter configuration table,
   see \sQuote{Format}}
}

\format{
The imported parameter configuration table requires the following fields:
\code{"Parameter"}, a unique identifier for each parameter and corresponding to
  field names in the data table;
\code{"Name"}, the local parameter name, used for legend text and axis labels;
\code{"Units"}, the parameter units used for axis labels;
\code{"pch"}, an integer specifying the symbol to use in plotting points;
\code{"col"}, the color code; and
\code{"bg"}, the background (fill) color for open plot symbols,
  \code{pch = 21:25}.
Each line of the table represents a single unique parameter.
}

\author{J.C. Fisher}

\seealso{\code{\link{legend}}}

\examples{
f <- system.file("extdata/Example_Config_Parameters.tsv", package = "Trends")
ShowParameters(f)
}

\keyword{hplot}
