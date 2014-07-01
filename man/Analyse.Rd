\name{Analyse}

\alias{RunAnalysis}
\alias{PlotObs}

\title{Analyse Observations}

\description{These functions analyse observations.}

\usage{
RunAnalysis(processed.obs, processed.config, path, id, sdate = NA, edate = NA,
            graphics.type = "pdf", merge.pdfs = TRUE, site.locations = NULL)

PlotObs(processed.obs, processed.config, path, id, sdate=NA, edate=NA,
        graphics.type = "pdf", merge.pdfs = TRUE)
}

\arguments{
\item{processed.obs}{something...}
\item{processed.config}{something...}
\item{path}{something...}
\item{id}{something...}
\item{sdate, edate}{character or Date; start and end date corresponding to the
  period of record, in YYYY-M-D (\code{\%Y-\%m-\%d}).}
\item{graphics.type}{character; see \code{\link{pdf}} and
  \code{\link{postscript}} for file-based devices.}
\item{merge.pdfs}{logical; if \code{TRUE} and \code{graphics.type} is
  \code{"pdf"}, plots are combined into a single PDF file, see
  \code{\link{MergePDFs}}.}
\item{site.locations}{something...}
}

\details{
something...
}

\value{
something... For \code{graphics.type} other than \code{"windows"}, this function is used
for the side-effect of drawing plots to external files.
}

\author{J.C. Fisher and L.C. Davis}

\seealso{\code{\link{ProcessObs}}, \code{\link{ProcessConfig}}}

\examples{
print("notyet")
}

\keyword{hplot}
