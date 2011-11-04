\name{ReadTrendData}

\alias{ReadTrendData}

\title{Read Trend Data}

\description{
Reads table formatted data from a text file and creates a data frame from it.
}

\usage{
ReadTrendData(file.data = NULL, initial.dir = getwd())
}

\arguments{
\item{file.data}{character; the file name which the data are to be read from.}
\item{initial.dir}{character; specifies that the initial directory when
  selecting a file to open, only used when \code{file.data} is \code{NULL}.}
}

\format{
The imported data table requires the following fields:
Site_ID, Well_name, Dates, Times
}

\details{
Something
}

\value{
Something
}

\author{Fisher, J.C., Davis, L.C.}

\seealso{
\code{\link{read.table}}
}

\examples{
\donttest{d <- ReadTrendData(file.data)}
}

\keyword{file}
