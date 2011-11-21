\name{RunTheilSen}

\alias{RunTheilSen}

\title{Run Theil-Sen Estimator}

\description{
This function computes the Theil-Sen estimator, nonparametric regression line,
and 95 percent confidence interval for uncensored data.
}

\usage{
RunTheilSen(x, y, alpha = 0.5, pr = FALSE)
}

\arguments{
\item{x}{numeric; a vector; such as the number of seconds since some
  origin date; for example, \code{"1920-01-01 00:00:00.0"}.}
\item{y}{numeric; a vector; such as parameter values.}
\item{alpha}{numeric; Type I probability error, sometimes referred to as the
  confidence of the test, or the level of significance of the test.}
\item{pr}{logical; should error messages be reported? Its default is
  \code{FALSE}.}
}

\details{
All of the code used in \code{RunTheilSen} is provided by Rand R. Wilcox
\url{http://www-rcf.usc.edu/~rwilcox/}.
The code format has been slightly altered for readability purposes and
packaged into a single wrapper function.
}

\value{
Returns a \code{list} with a single 2 by 5 \code{matrix} component,
\code{regci}. The matrix has column names of \code{"ci.low"},
\code{"ci.up"}, \code{"Estimate"}, \code{"S.E."}, and \code{"p-value"};
and row names of \code{"intercept"} and \code{"X"}. The \emph{p}-value for
the Theil-Sen estimator \code{[2,5]}; slope \code{[2,3]} and
intercept \code{[1,3]} of the regression line; slope \code{[2,1]} and
intercept \code{[1,1]} of the lower confidence interval; and
slope \code{[2,2]} and intercept \code{[1,2]} of the upper
confidence interval are included in the matrix.
}

\author{R.R. Wilcox and J.C. Fisher}

\references{
Wilcox, R.R., 2012, \emph{Modern Statistics for the Social and Behavioral
Sciences: A Practical Introduction}, New York, CRC Press, 862 p.
}

\examples{
x <- c(504946800, 536482800, 694249200, 725871600, 757407600, 788943600,
       820479600, 852102000, 883638000, 915174000, 946710000, 978332400)
y <- c(12.000, 12.000, 15.000, 12.000, 14.000, 15.000, 16.508, 19.625,
       18.894, 20.500, 18.830, 21.540)
out <- RunTheilSen(x, y)$regci
}

\keyword{misc}
