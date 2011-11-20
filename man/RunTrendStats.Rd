\name{RunTrendStats}

\alias{RunTrendStats}

\title{Open Graphics Device}

\description{
This function performs a statistical analysis on censored and uncensored data.
}

\usage{
RunTrendStats(d, site.names, is.censored = FALSE, initial.dir = getwd(),
              file.par = NULL, file.stats = NULL, write.tbl.out = FALSE,
              file.out = NULL, figs.dir = NULL, avg.time = "year",
              gr.type = "pdf", cenken.tol = 1e-12, cenken.iter = 1e+6)
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadTrendData}}.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the statistics configuration file are used.}
\item{is.censored}{logical; if \code{TRUE}, the data is assumed to be
  censored, its default is \code{FALSE}, see \sQuote{Details}.}
\item{initial.dir}{character; specifies the initial directory in the open files
  pop-up box.}
\item{file.par}{character; file path for the parameters
  configuration table, see \code{\link{ShowParameters}}.}
\item{file.stats}{character; file path for the statistics configuration table,
  see \sQuote{Format}.}
\item{write.tbl.out}{logical; if \code{TRUE}, the output table
  is written to a text file, its default is \code{FALSE}.}
\item{file.out}{character; file path for the statistics summary table,
  see \sQuote{Value}.}
\item{figs.dir}{character; if \code{gr.type} not equal to \code{"windows"},
  the directory where statistic plots are saved.}
\item{avg.time}{character; the time interval into which the period of record
  is to be cut.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
\item{cenken.tol}{numeric; numerical tolerance, see \sQuote{Details}.}
\item{cenken.iter}{numeric; maximum number of iterations, see \sQuote{Details}.}
}

\format{
The imported statistics configuration table requires the following fields:
\code{"Site_id"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site identifier;
\code{"Parameters"}, comma seperated parameter names corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Start_date"}, start date in M/D/YYYY (\code{\%m/\%d/\%Y});
\code{"End_date"}, end date; and
\code{"Remark"}, any comments (optional).
}

\details{
The censored data is analyzed using functions provided in the \pkg{NADA}
package. Summary statistics are calculated using \code{\link{cenfit}} and the
Kendalls tau correlation coefficient and associated Akritas-Theil-Sen (ATS)
nonparametric regression line are calculated using \code{kendallATS},
a hidden function in \pkg{NADA}.
The ATS slope estimation uses an iterative bisection search
with a stopping criteria defined by \code{cenken.tol} and \code{cenken.iter}.

The Theil-Sen estimator and trend line for uncensored data is calculated
using robust statistic functions provided by R.R. Wilcox,
see \code{\link{RunTheilSen}} for details.

A new graphics device is opened for each site and after four plots have
been drawn.
}

\value{
Returns a data frame with variables:
\item{Site_id}{numeric; a unique numerical identifier used to access site data.}
\item{Site_name}{character; a local site identifier.}
\item{Parameter}{character; something.}
\item{Start_date}{POSIXct; something.}
}

\author{J.C. Fisher and L.C. Davis}

\references{
Helsel, D.R., 2005, \emph{Nondectects and Data Analysis; Statistics for
censored environmental data}. John Wiley and Sons, USA, NJ.

Lorenz, D.L., and others, 2011, USGS library for S-PLUS for Windows --
Release 4.0: U.S. Geological Survey Open-File Report 2011-1130.
}

\examples{
f <- system.file("extdata/Example_Data.txt", package = "Trends")
d <- ReadTrendData(f)
file.par <- system.file("extdata/Example_Config_Parameters.txt",
                        package = "Trends")

file.stats <- system.file("extdata/Example_Config_Censored.txt",
                          package = "Trends")
out <- RunTrendStats(d, site.names = c("ANP 9", "HIGHWAY 3"),
                     is.censored = TRUE, file.par = file.par,
                     file.stats = file.stats, gr.type="windows")

file.stats <- system.file("extdata/Example_Config_Uncensored.txt",
                          package = "Trends")
out <- RunTrendStats(d, site.names = c("ANP 6", "ATOMIC CITY"),
                     is.censored = FALSE, file.par = file.par,
                     file.stats = file.stats, gr.type="windows")
}

\keyword{misc}
