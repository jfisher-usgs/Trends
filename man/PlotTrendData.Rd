\name{PlotTrendData}

\alias{PlotTrendData}

\title{Plot Trend Data}

\description{This function queries and plots trend data.}

\usage{
PlotTrendData(d, site.names, sdate = NA, edate = NA, initial.dir = getwd(),
              file.par = NULL, file.plots = NULL, figs.dir = NULL,
              gr.type = "pdf")
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadTrendData}}.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the plots configuration file are used.}
\item{sdate, edate}{character; start and end date corresponding to the
  period of record, in M/D/YYYY (\code{\%m/\%d/\%Y}).}
\item{initial.dir}{character; specifies the initial directory in the Open Files
  pop-up box; only used when \code{file.par} or \code{file.plots} is
  \code{NULL}.}
\item{file.par}{character; file path for the parameters
  configuration table, see \code{\link{ShowParameters}}.}
\item{file.plots}{character; file path for the plot configuration table,
  see \sQuote{Format}.}
\item{figs.dir}{character; the directory where data plots are saved,
  only applicable when \code{gr.type} is not equal to \code{"windows"}.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
}

\format{
The imported plot configuration table requires the following fields:
\code{"Site_id"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site name;
\code{"Parameters"}, comma separated parameter identifiers corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Min"}, the minimum \code{y}-axis plot limit;
\code{"Max"}, the maximum \code{y}-axis plot limit; and
\code{"Axis_title"}, the \code{y}-axis title.
Each line of the table represents a single plot with unique point symbols drawn
for each parameter in the \code{"Parameters"} field.
}

\details{
A new graphics device is opened for each site and after four plots have
been drawn.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{DrawPlot}}}

\examples{
f <- system.file("extdata/Example_Data.tsv", package = "Trends")
d <- ReadTrendData(f)

file.par <- system.file("extdata/Example_Config_Parameters.tsv",
                        package = "Trends")
file.plots <- system.file("extdata/Example_Config_Plots.tsv",
                          package = "Trends")

PlotTrendData(d, site.names = c("ANP 6", "ARBOR TEST"),
              sdate = "01/01/1990", edate = "01/01/2010",
              file.par = file.par, file.plots = file.plots,
              gr.type = "windows")
}

\keyword{hplot}
