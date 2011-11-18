\name{DrawPlot}

\alias{DrawPlot}

\title{Draw Single Plot}

\description{
This function draws a single plot on a graphics device.
}

\usage{
DrawPlot(d, tbl.par, cen.var = NULL,
         xlim = c(NA, NA), ylim = c(NA, NA),
         regr = NULL, regr.lower = NULL, regr.upper = NULL,
         regr.type = "Regression line",
         main = NULL, xlab = NULL, ylab = NULL,
         leg.box.col="#FFFFFF", tick.lines = TRUE, p.value = NULL)
}

\arguments{
\item{d}{data.frame; something.}
\item{tbl.par}{data.frame; something.}
\item{cen.var}{character or integer; something.}
\item{xlim}{POSIXct; something.}
\item{ylim}{numeric; something.}
\item{regr}{function; something.}
\item{regr.lower}{function; something.}
\item{regr.upper}{function; something.}
\item{regr.type}{character; something.}
\item{main}{character; something.}
\item{xlab}{character; something.}
\item{ylab}{character; something.}
\item{leg.box.col}{character; something.}
\item{tick.lines}{logical; something.}
\item{p.value}{numeric; something.}
}

\details{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link{plot.window}}, \code{\link{axis.POSIXct}}, \code{\link{legend}}
}

\examples{
print("something")
}

\keyword{hplot}
