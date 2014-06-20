\name{PlotObservations}

\alias{PlotObservations}

\title{Plot Observations}

\description{This function queries and plots trend data.}

\usage{
PlotObservations(d, site.names, parameters, plot.config, sdate = NA, edate = NA,
                 path.out = tempfile(""), gr.type = "pdf", merge.pdfs = FALSE)
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadObservations}}.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the plots configuration file are used.}
\item{parameters}{data.frame; parameter configuration table, see
  \code{\link{ReadParConfig}}.}
\item{plot.config}{data.frame; plot configuration table, see
  \code{\link{ReadPlotConfig}}.}
\item{sdate, edate}{character; start and end date corresponding to the
  period of record, in M/D/YYYY (\code{\%m/\%d/\%Y}).}
\item{path.out}{character; the directory where data plots are saved,
  only applicable when \code{gr.type} is not equal to \code{"windows"}.}
\item{gr.type}{character; see \code{\link{OpenDevice}}, its default is
  \code{"pdf"}.}
\item{merge.pdfs}{logical; if \code{TRUE} and \code{gr.type} is \code{"pdf"},
  plots are combined into a single PDF file, see \code{\link{MergePDFs}}.}
}

\details{
Each row of \code{plot.config} represents a single plot with unique point
symbols drawn for each parameter in its \code{"Parameters"} component.
A new graphics device is opened for each site or after four consecutive plots
have been drawn for the same site.
}

\value{
None. For \code{gr.type} other than \code{"windows"}, this function is used
for the side-effect of drawing plots to external files.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{DrawPlot}}}

\examples{
f <- system.file("extdata/ex.data.tsv", package = "Trends")
d <- ReadObservations(f)

file.par <- system.file("extdata/ex.parameters.tsv", package = "Trends")
parameters <- ReadParConfig(file.par)

file.plot <- system.file("extdata/ex.config.plots.tsv", package = "Trends")
plot.config <- ReadPlotConfig(file.plot)

PlotObservations(d, site.names = c("ANP 6", "ARBOR TEST"), parameters = parameters,
                 plot.config = plot.config, sdate = "01/01/1990", edate = "01/01/2010",
                 gr.type = "windows")
}

\keyword{hplot}
