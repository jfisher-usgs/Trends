\name{ReadObservations}

\alias{ReadObservations}

\title{Read Observation File}

\description{
This function reads table formatted observation data from a text file and
creates a data frame from it.
}

\usage{ReadObservations(file)}

\arguments{
\item{file}{character; the file path which the data are to be read from.}
}

\format{
The imported data table requires the following field names:
\code{"Site_id"}, \code{"Site_name"}, \code{"Dates"}, and \code{"Times"}.
The required date and time format is M/D/YYYY (\code{\%m/\%d/\%Y}) and
HM (\code{\%H\%M}), respectively. Parameter field names are arbitrary; however,
they must be consistent with names specified in the configuration files.
}

\details{
Parameter fields in the imported data table are read as
class \code{character} and are checked for character codes in the first digit of
each value. These fields are stripped of their single digit character
codes and converted to class \code{numeric}. A new component is added to the
data frame and populated with integer codes representing their respective
character code. The variable name for this new component is based on a
concatenation of the parameter name and \code{"_code"}.
Codes are identified using the following criteria:
\tabular{cll}{"0-9", "." \tab No character code \tab 0\cr
"<" \tab Less than, censored data \tab 1\cr
"E" \tab Estimated value \tab 2\cr
"V" \tab Sample contaminated \tab 3\cr
"U" \tab Undetectable \tab 4}
A warning is given if the character code
is not recognized and its value set to \code{NA}. Values with character
codes of \code{"V"} and \code{"U"} are set to \code{NA}.
}

\value{
Returns a data frame with components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{numeric; a local site name.}
\item{Datetime}{POSIXct; the date and time corresponding to when the
  sample was collected.}
\item{...}{numeric; parameter fields and integer codes.}
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{read.table}}}

\examples{
f <- system.file("extdata/ex.data.tsv", package = "Trends")
d <- ReadObservations(f)
}

\keyword{file}
