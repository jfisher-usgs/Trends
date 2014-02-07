\name{RunTrendStats}

\alias{RunTrendStats}

\title{Open Graphics Device}

\description{
This function performs a statistical analysis on censored and uncensored data.
}

\usage{
RunTrendStats(d, site.names, file.par, file.stats, is.censored = FALSE,
              write.tbl.out = FALSE, file.out = NULL, figs.dir = getwd(),
              gr.type = "pdf", cenken.tol = 1e-12, cenken.iter = 1e+6,
              dt.breaks = NULL, xout = FALSE, draw.ci = FALSE)
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadTrendData}}.}
\item{site.names}{character; an array of local site names; if missing,
  all sites in the statistics configuration file are used.}
\item{file.par}{character; file path for the parameters
  configuration table, see \code{\link{ShowParameters}}.}
\item{file.stats}{character; file path for the statistics configuration table,
  see \sQuote{Format}.}
\item{is.censored}{logical; if \code{TRUE}, the data is assumed to be
  censored, its default is \code{FALSE}, see \sQuote{Details}.}
\item{write.tbl.out}{logical; if \code{TRUE}, the output table
  is written to a text file, its default is \code{FALSE}.}
\item{file.out}{character; file path for the statistics summary table,
  see \sQuote{Value}.}
\item{figs.dir}{character; if \code{gr.type} not equal to \code{"windows"},
  the directory where statistic plots are saved.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
\item{cenken.tol}{numeric; a tolerance used as a stopping criteria,
  see \sQuote{Details}.}
\item{cenken.iter}{numeric; maximum number of iterations, see \sQuote{Details}.}
\item{dt.breaks}{character; an interval specification, one of \code{"sec"},
  \code{"min"}, \code{"hour"}, \code{"day"}, \code{"week"}, \code{"month"},
  \code{"quarter"} or \code{"year"}. Parameter values are averaged over this
  time interval.}
\item{xout}{logical; if \code{TRUE}, leverage points for uncensored data are
  removed based on an outlier detection method.}
\item{draw.ci}{logical; if \code{TRUE}, 95 percent confidence interval for
  uncensored data is drawn on plot.}
}

\format{
The imported statistics configuration table requires the following fields:
\code{"Site_id"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site identifier;
\code{"Parameters"}, comma separated parameter names corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Start_date"} and \code{"End_date"}, the start and end date in M/D/YYYY
  (\code{\%m/\%d/\%Y}); and
\code{"Remark"}, user comments (optional).
}

\details{
The censored data is analyzed using functions provided in the \pkg{NADA}
package. Summary statistics are calculated using \code{\link{cenfit}} and the
Kendall's tau correlation coefficient and associated Akritas-Theil-Sen (ATS)
nonparametric regression line are calculated using \code{kendallATS},
a hidden function in \pkg{NADA} (see \code{\link{cenken}}).
The ATS slope estimation uses an iterative bisection search
with a stopping criteria defined by \code{cenken.tol} and \code{cenken.iter};
\pkg{NADA} default values for these arguments are \code{1e-7} and
\code{1e+3}, respectively.

The Theil-Sen estimator and trend line for uncensored data is calculated
using robust statistic functions provided by R.R. Wilcox,
see \code{\link{RunTheilSen}} for details.

A new graphics device is opened for each site and after four plots have
been drawn.
}

\value{
Returns a data frame with variables:
\item{Site_id}{factor; a unique identifier used to access site data.}
\item{Site_name}{character; a local site name.}
\item{Parameter}{character; parameter identifier.}
\item{Start_date}{POSIXct; lower temporal limit.}
\item{End_date}{POSIXct; upper temporal limit.}
\item{n}{integer; number of non-\code{NA} observations.}
\item{n_above_rl}{integer; number of observations with values above the
  recording limit, that is values coded with \sQuote{<} or \sQuote{E},
  see \code{\link{ReadTrendData}}.}
\item{n_cen}{integer; number of observations with censored values.}
\item{mean}{numeric; mean.}
\item{median}{numeric; median.}
\item{min}{numeric; minimum value.}
\item{max}{numeric; maximum value.}
\item{std_dev}{numeric; standard deviation.}
\item{len_record}{difftime; period of record.}
\item{p_value}{numeric; estimated two-sided \emph{p}-value, if below the
  acceptance level (5 percent), reject the null hypothesis that the parameter is
  statistically independent of time and accept the hypothesis that they are
  dependent.}
\item{tau}{numeric; Kendall's tau (censored only).}
\item{slope}{numeric; slope of nonparametric regression line, in
  parameter change per year.}
\item{lower}{numeric; slope of lower confidence interval (uncensored only).}
\item{upper}{numeric; slope of upper confidence interval (uncensored only).}
\item{int}{numeric; intercept of nonparametric regression line.}
\item{int_lower}{numeric; intercept of lower confidence interval
  (uncensored only).}
\item{int_upper}{numeric; intercept of upper confidence interval
  (uncensored only).}
\item{slope_percent}{numeric; percent change per year of nonparametric
  regression line.}
\item{lower_percent}{numeric; percent change per year of lower confidence
  interval (uncensored only).}
\item{upper_percent}{numeric; percent change per year of upper confidence
  interval (uncensored only).}
\item{trend}{character; significant trends are indicated by a \emph{p}-value
  less than or equal to 0.05, a 5 percent acceptance level.
  The sign of the \code{slope} indicates whether the significant trend is
  positive (\code{"+"}) or negative (\code{"-"}). \emph{p}-values greater
  than 0.05 are specified as \code{"no trend"}.}
}

\author{J.C. Fisher and L.C. Davis}

\references{
Helsel, D.R., 2005, \emph{Nondectects and Data Analysis; Statistics for
censored environmental data}, New Jersey, John Wiley and Sons, Inc., 250 p.

Lorenz, D.L., and others, 2011, USGS library for S-PLUS for Windows --
Release 4.0: U.S. Geological Survey Open-File Report 2011-1130.
}

\examples{
f <- system.file("extdata/ex.data.tsv", package = "Trends")
d <- ReadTrendData(f)
file.par <- system.file("extdata/ex.config.parameters.tsv", package = "Trends")

file.stats <- system.file("extdata/ex.config.censored.tsv", package = "Trends")
out <- RunTrendStats(d, site.names = c("ANP 9", "HIGHWAY 3"),
                     file.par = file.par, file.stats = file.stats,
                     is.censored = TRUE, gr.type = "windows")

file.stats <- system.file("extdata/ex.config.uncensored.tsv", package = "Trends")
out <- RunTrendStats(d, site.names = c("ANP 6", "ATOMIC CITY"),
                     file.par = file.par, file.stats = file.stats,
                     is.censored = FALSE, gr.type = "windows", draw.ci = TRUE)
}

\keyword{misc}
