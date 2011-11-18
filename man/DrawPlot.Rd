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
         main = NULL, ylab = NULL, leg.box.col="#FFFFFF",
         tick.lines = TRUE, p.value = NULL)
}

\arguments{
\item{d}{data.frame; something.}
\item{tbl.par}{data.frame; parameter configuration table,
  see \code{\link{ShowParameters}}.}
\item{cen.var}{character or integer; the name or index number of the
  logical component in \code{d} that indicates whether observations are
  censored.}
\item{xlim}{POSIXct; the \code{x} limits (\code{x1}, \code{x2}) of the plot.}
\item{ylim}{numeric; the \code{y} limits.}
\item{regr}{function; regression.}
\item{regr.lower}{function; lower confidence interval for regression.}
\item{regr.upper}{function; upper confidence interval for regression.}
\item{regr.type}{character; a legend label for the regression line.}
\item{main}{character; a main title for the plot.}
\item{ylab}{character; a label for the \code{y} axis.}
\item{leg.box.col}{character; the background color for the legend box.}
\item{tick.lines}{logical; if \code{TRUE}, horizontal and vertical lines are
  drawn at the major tick marks.}
\item{p.value}{numeric; the p-value for the regression.}
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
