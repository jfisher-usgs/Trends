\name{ShowSymbols}

\alias{ShowSymbols}

\title{Draw All Possible Point Symbols}

\description{
Draw all possible point symbols within a legend.
}

\usage{
ShowSymbols(file.symbs)
}

\arguments{
\item{file.symbs}{character; file path for the point symbol configuration
   table, see \sQuote{Format}}
}

\format{
The imported point symbol configuration table requires the following fields:
\code{"Constituent"}, a unique parameter name corresponding to header fields
  in the data table;
\code{"Name"}, the parameter name, used for legend text and axis labels;
\code{"pch"}, an integer specifying the symbol to use in plotting points;
\code{"col"}, the color code; and
\code{"bg"}, the background (fill) color for the open plot symbols given by
  \code{pch = 21:25}.
Each line of the table represents a single unique parameter.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{legend}}
}

\examples{
f <- system.file("extdata/Example_Config_Symbols.txt", package = "Trends")
ShowSymbols(f)
}

\keyword{hplot}
