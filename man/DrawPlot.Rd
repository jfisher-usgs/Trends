\name{DrawPlot}

\alias{DrawPlot}

\title{Draw Single Plot}

\description{This function draws a single plot on the active graphics device.}

\usage{DrawPlot(d, model, xlim = NULL, ylim = NULL, main = NULL, ylab = "")}

\arguments{
\item{d}{\code{data.frame}; observations, where the first and second column is of class \code{Date} and \code{\link{Surv}}, respectively.
  Right-censored data is not permitted.}
\item{model}{\code{\link{survreg}}; the survival regression model of \emph{interval} type (optional).}
\item{xlim}{\code{Date}; the \code{x} limits of the plot.}
\item{ylim}{\code{numeric}; the \code{y} limits of the plot.}
\item{main}{\code{character}; a main title for the plot.}
\item{ylab}{\code{character}; a label for the \code{y} axis.}
}

\details{
Exact data is drawn as points.
Left- and interval-censored data is drawn as vertical lines with short-horizontal lines at the interval bounds.
The 50th percentile of the regression model is drawn as a curved line and the interval between the 10th and 90th confidence bands as a solid polygon.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\examples{
# Create time-series data set with exact, left-censored, and interval-censored data
n <- 30
x <- as.Date(sort(sample(1:1000, n)), origin = as.Date("1992-02-15"))
time1 <- runif(n, min = 0, max = 100)
time2 <- time1 + runif(n, min = 0, max = 10)
time1[sample(1:n, 10)] <- NA
idxs <- sample(1:n, 10)
time1[idxs] <- time2[idxs]
y <- survival::Surv(time1, time2, type = "interval2")
d <- data.frame(x, y)

# Fit the data with a regression model
model <- survival::survreg(y ~ x, data = d)

# Plot the data and regression model
DrawPlot(d, model, main = "Title", ylab = "Value")
}

\keyword{hplot}
