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
\item{file.data}{character; the file path which the data are to be read from.}
\item{initial.dir}{character; specifies the initial directory in the open files
  pop-up box; only used when \code{file.data} is \code{NULL}.}
}

\format{
The imported data table requires the following field names:
\code{"Site_id"}, \code{"Site_name"}, \code{"Dates"}, and \code{"Times"}.
Parameter field names are arbitrary; however, they must be consistent among
input tables. See \sQuote{Value} for information on field content.
}

\details{
Parameter fields in the imported data table read as
class \code{character} are checked for character codes in the first digit of
each value. These fields are stripped of their single digit character
codes and converted to class \code{numeric}. A new component is added to the
data frame and populated with integer codes representing their respective
character code. The variable name for this new component is based on a
concatenation of the parameter name and \code{"_code"}.
Codes are identified using the following criteria:
\tabular{cll}{"0-9", "." \tab No character code \tab 0\cr
"<" \tab Less than \tab 1\cr
"E" \tab Estimated value \tab 2\cr
"V" \tab Sample contaminated \tab 3\cr
"U" \tab Undetectable \tab 4}
A warning is given if the character code
is not recognized and its value set to \code{NA}. Values with character
codes of \code{"V"} and \code{"U"} are also set to \code{NA}.
}

\value{
Returns a data frame with components:
\item{Site_id}{numeric; a unique numerical identifier used to access site data.}
\item{Site_name}{numeric; a local site identifier.}
\item{Dates}{character; date sample was collected M/D/YYYY (\%m/\%d/\%Y).}
\item{Times}{character; time sample was collected in HM (\%H\%M).}
\item{...}{numeric; parameter fields and integer codes.}
}

\author{Fisher, J.C., Davis, L.C.}

\seealso{
\code{\link{read.table}}
}

\examples{
f <- system.file("extdata/Example_Data.txt", package = "Trends")
d <- ReadTrendData(f)
head(d)
}

\keyword{file}
