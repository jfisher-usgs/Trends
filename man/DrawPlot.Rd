\name{DrawPlot}

\alias{DrawPlot}

\title{Draw Single Plot}

\description{This function draws a single plot on a graphics device.}

\usage{DrawPlot(d, model, xlim = NULL, ylim = NULL, main = NULL, ylab = "")}

\arguments{
\item{d}{\code{\link{data.frame}}; observations, the first and second column is of class \code{\link{Date}} and \code{\link{Surv}}, respectively.
  Right censored data is not permitted.}
\item{model}{\code{\link{survreg}}; survival regression model of \emph{interval} type (optional).}
\item{xlim}{Date; the \code{x} limits (\code{x1}, \code{x2}) of the plot.}
\item{ylim}{numeric; the \code{y} limits (\code{y1}, \code{y2}) of the plot.}
\item{main}{character; a main title for the plot.}
\item{ylab}{character; a label for the \code{y} axis.}
}

\details{
Exact data is drawn as points.
Left and interval censored data is drawn as vertical lines.
The 50th percentile of the regression model is drawn as a curved line and the interval between the 10th and 90th confidence bands is drawn as a solid polygon.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\examples{
n <- 30
x <- as.Date(sort(sample(1:1000, n)), origin = as.Date("1992-02-15"))
time1 <- runif(n, min = 0, max = 100)
time2 <- time1 + runif(n, min = 0, max = 10)
time1[sample(1:n, 10)] <- NA
idxs <- sample(1:n, 10)
time1[idxs] <- time2[idxs]
y <- survival::Surv(time1, time2, type = "interval2")

d <- data.frame(x, y)
model <- survival::survreg(y ~ x, data = d)
DrawPlot(d, model, main = "Title", ylab = "Value")
}

\keyword{hplot}
