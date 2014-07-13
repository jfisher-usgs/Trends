\name{DrawPlot}

\alias{DrawPlot}

\title{Draw Single Plot}

\description{This function draws a single time-series plot on the active graphics device.}

\usage{
DrawPlot(d, model, plim = c(0.1, 0.9), xlim = NULL, ylim = NULL,
         main = NULL, ylab = "")
}

\arguments{
\item{d}{\code{data.frame}; observations, where the first and second column is of class \code{Date} and \code{\link{Surv}}, respectively.
  The \code{Surv} component is of type \emph{interval} and should not contain right-censored data.}
\item{model}{\code{\link{survreg}}; a survival regression model.}
\item{plim}{\code{numeric}; the percentile limits defining the confidence band of the regression model.}
\item{xlim}{\code{Date}; the \code{x} limits of the plot.}
\item{ylim}{\code{numeric}; the \code{y} limits of the plot.}
\item{main}{\code{character}; a main title for the plot.}
\item{ylab}{\code{character}; a label for the \code{y} axis.}
}

\details{
Uncensored data is drawn as points.
Left- and interval-censored data is drawn as vertical lines with short-horizontal lines at the interval bounds.
Note that the lower bound of left-censored data is placed at zero.
The 50th percentile of the regression model is drawn as a curved line.
The confidence band is drawn as a solid polygon.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}, \code{\link[survival]{predict.survreg}}}

\examples{
# Create time-series data set with uncensored, left-censored, and interval-censored data
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
DrawPlot(d, model, plim = c(NA, 0.8), main = "Title", ylab = "Value")
}

\keyword{hplot}
