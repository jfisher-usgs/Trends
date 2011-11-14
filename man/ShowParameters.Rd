\name{ShowParameters}

\alias{ShowParameters}

\title{Draw All Parameters in a Legend}

\description{
Draw all possible parameters within a legend.
}

\usage{
ShowParameters(file.parameters = NULL)
}

\arguments{
\item{file.parameters}{character; file path for the parameter configuration
   table, see \sQuote{Format}}
}

\format{
The imported parameter configuration table requires the following fields:
\code{"Parameter"}, a unique parameter name corresponding to header fields
  in the data table;
\code{"Name"}, the parameter name, used for legend text and axis labels;
\code{"pch"}, an integer specifying the symbol to use in plotting points;
\code{"col"}, the color code; and
\code{"bg"}, the background (fill) color for the open plot symbols given by
  \code{pch = 21:25}.
Each line of the table represents a single unique parameter.
}

\author{J.C. Fisher}

\seealso{
\code{\link{legend}}
}

\examples{
f <- system.file("extdata/Example_Config_Parameters.txt",
                 package = "Trends")
ShowParameters(f)
}

\keyword{hplot}
