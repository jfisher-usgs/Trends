\name{DrawPlot}

\alias{DrawPlot}

\title{Draw Single Plot}

\description{This function draws a single plot on a graphics device.}

\usage{
DrawPlot(obj, model, xlim = NULL, ylim = NULL, main = NULL, ylab = NULL)
}

\arguments{
\item{obj}{data.frame; something...}
\item{model}{survreg; something...}
\item{xlim}{POSIXct; the \code{x} limits (\code{x1}, \code{x2}) of the plot.}
\item{ylim}{numeric; the \code{y} limits (\code{y1}, \code{y2}) of the plot.}
\item{main}{character; a main title for the plot.}
\item{ylab}{character; a label for the \code{y} axis.}
}

\details{
Exact data is plotted as points and censored data as vertical lines.
Regression and 10th and 90th percentile interval is drawn if \code{model}
is not missing.
}

\author{J.C. Fisher}

\seealso{\code{\link{PlotObs}}, \code{\link{RunAnalysis}}}

\examples{
print("notyet")
}

\keyword{hplot}
