\name{PlotTrendData}

\alias{PlotTrendData}

\title{Plot Trend Data}

\description{
This function queries and plots trend data.
}

\usage{
PlotTrendData(d, site.names, sdate = NA, edate = NA, initial.dir = getwd(),
              file.parameters = NULL, file.plots = NULL, figs.dir = NULL,
              gr.type = "pdf")
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadTrendData}}.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the plots configuration file are used.}
\item{sdate, edate}{character; start and end date corresponding to the
  period of record, in M/D/YYYY (\%m/\%d/\%Y).}
\item{initial.dir}{character; specifies the initial directory in the open files
  pop-up box; only used when \code{file.parameters} or \code{file.plots} is
  \code{NULL}.}
\item{file.parameters}{character; file path for the parameters
  configuration table, see \code{\link{ShowParameters}}.}
\item{file.plots}{character; file path for the plot configuration table,
  see \sQuote{Format}.}
\item{figs.dir}{character; if \code{gr.type} not equal to \code{"windows"},
  the directory where data plots are saved.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
}

\format{
The imported plot configuration table requires the following fields:
\code{"Site_id"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site identifier;
\code{"Parameters"}, comma seperated parameter names corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Min"}, the minimum y-axis plot limit;
\code{"Max"}, the maximum y-axis plot limit; and
\code{"Axis_title"}, the y-axis title.
Each line of the table represents a single plot, with its point-plotted
data sets based on parameters in \code{"Parameters"}.
}

\details{
A new graphics device is opened for each site and after four plots have
been drawn.
}

\author{Fisher, J.C., Davis, L.C.}

\seealso{
\code{\link{DrawPlot}}
}

\examples{
f <- system.file("extdata/Example_Data.txt", package = "Trends")
d <- ReadTrendData(f)

file.parameters <- system.file("extdata/Example_Config_Parameters.txt",
                               package = "Trends")
file.plots <- system.file("extdata/Example_Config_Plots.txt",
                          package = "Trends")

PlotTrendData(d, site.names = c("ANP 6", "ARBOR TEST"),
              sdate = "01/01/1990", edate = "01/01/2010",
              file.parameters = file.parameters, file.plots = file.plots,
              gr.type = "windows")
}

\keyword{hplot}
