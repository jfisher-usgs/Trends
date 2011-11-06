\name{PlotTrendData}

\alias{PlotTrendData}

\title{Plot Trend Data}

\description{
This function draws plots on the desired device type.
}

\usage{
PlotTrendData(d, site.names, sdate = NA, edate = NA, initial.dir = getwd(),
              file.symbs = NULL, file.plots = NULL, figs.dir = NULL,
              gr.type = "pdf")
}

\arguments{
\item{d}{data.frame; something.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the plots configuration file are used.}
\item{sdate, edate}{character; start and end date corresponding to the
  period of record, in M/D/YYYY (\%m/\%d/\%Y).}
\item{initial.dir}{character; specifies the initial directory in the open files
  pop-up box; only used when \code{file.symbs} or \code{file.plots} is
  \code{NULL}.}
\item{file.symbs}{character; file path for the point symbol configuration
   table, see \code{\link{ShowSymbols}}.}
\item{file.plots}{character; file path for the plot configuration table,
  see \sQuote{Format}.}
\item{figs.dir}{character; if \code{gr.type} not equal to \code{"windows"},
  the directory where data plots are saved.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
}

\format{
The imported plot configuration table requires the following fields:
\code{"Site_ID"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site identifier;
\code{"Constituents"}, comma seperated parameter names corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Min"}, the minimum y-axis plot limit;
\code{"Max"}, the maximum y-axis plot limit; and
\code{"Axis_title"}, the y-axis title.
Each line of the table represents a single plot, with its point-plotted
data sets based on parameters in \code{"Constituents"}.
}

\details{
Something
}

\author{Fisher, J.C., Davis, L.C.}

\seealso{
\code{\link{plot.window}}, \code{\link{axis.POSIXct}}, \code{\link{legend}}
}

\examples{
\donttest{PlotTrendData()}
}

\keyword{hplot}
