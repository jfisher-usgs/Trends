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
         main = NULL, ylab = NULL, leg.box.col = "#FFFFFF",
         tick.lines = TRUE, p.value = NULL)
}

\arguments{
\item{d}{data.frame; coordinate vectors of points to plot and a logical vector
  indicating those values that are censored. The first component contains
  date and time values of class \code{POSIXct}.}
\item{tbl.par}{data.frame; parameter configuration table, see
  \code{\link{ShowParameters}}.}
\item{cen.var}{character or integer; the name or index number of the
  logical component in \code{d} indicating whether observations are censored.}
\item{xlim}{POSIXct; the \code{x} limits (\code{x1}, \code{x2}) of the plot.}
\item{ylim}{numeric; the \code{y} limits (\code{y1}, \code{y2}) of the plot.}
\item{regr}{function; a regression line, only drawn for single parameter plots.}
\item{regr.lower}{function; lower confidence interval for regression.}
\item{regr.upper}{function; upper confidence interval for regression.}
\item{regr.type}{character; a label for the regression line, shown in legend.}
\item{main}{character; a main title for the plot.}
\item{ylab}{character; a label for the \code{y} axis.}
\item{leg.box.col}{character; the background color for the legend box with
  semi-transparency supported for some graphics devices.}
\item{tick.lines}{logical; if \code{TRUE}, horizontal and vertical lines are
  drawn at the major tick marks.}
\item{p.value}{numeric; the estimated \emph{p}-value of the regression line.}
}

\details{
Uncensored data is plotted as points and censored data as vertical dashed lines.
Regression and confidence interval lines are only drawn if \code{d} contains
a single parameter.
}

\author{J.C. Fisher}

\seealso{
\code{\link{plot.window}}, \code{\link{axis}}, \code{\link{axis.POSIXct}},
\code{\link{abline}}, \code{\link{polygon}}, \code{\link{lines}},
\code{\link{points}}, \code{\link{legend}}
}

\examples{
f <- system.file("extdata/Example_Data.txt", package = "Trends")
d <- ReadTrendData(f)
d <- d[d$Site_id == 433657112563601, ]

f <- system.file("extdata/Example_Config_Parameters.txt", package = "Trends")
p <- read.table(file = f, header = TRUE, sep = "\t",
                stringsAsFactors = FALSE, comment.char = "", row.names = 1)

windows()

d1 <- d[, c("Datetime", "Temp", "AS")]
DrawPlot(d1, tbl.par = p, main = "Uncensored Data with Multiple Parameters",
         ylab = "Concentration, in milligrams per liter",
         leg.box.col = "#FFFFFFBB")

d2 <- d[, c("Datetime", "AS")]
fit <- lm(AS ~ Datetime, d2)
b <- fit$coefficients[[1]]
m <- fit$coefficients[[2]]
regr <- function(x) {m * as.numeric(x) + b}
regr.lower <- function(x) {m * as.numeric(x) + b - 0.1 * b}
regr.upper <- function(x) {m * as.numeric(x) + b + 0.1 * b}
DrawPlot(d2, tbl.par = p, regr = regr,
         regr.lower = regr.lower, regr.upper = regr.upper,
         regr.type = "Simple linear regression",
         main = "Uncensored Data with Single Parameter and Trend Lines",
         ylab = "Arsenic, in micrograms per liter")

d3 <- d[, c("Datetime", "AS", "AS_code")]
d3[, "AS_code"] <- d3[, "AS_code"] == 1
DrawPlot(d3, tbl.par = p, cen.var = 3, xlim = c("1/1/1985", NA), regr = regr,
         main = "Censored Data with Single Parameter and Trend Line",
         ylab = "Arsenic, in micrograms per liter")
}

\keyword{hplot}
