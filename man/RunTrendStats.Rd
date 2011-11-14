\name{RunTrendStats}

\alias{RunTrendStats}

\title{Open Graphics Device}

\description{
This function performs a statistical analysis on uncensored and censored data.
}

\usage{
RunTrendStats(d, site.names, is.censored = FALSE, initial.dir = getwd(),
              file.parameters = NULL, file.stats = NULL, file.out = NULL,
              figs.dir = NULL, avg.time = "year", gr.type = "pdf")
}

\arguments{
\item{d}{data.frame; trend data, see \code{\link{ReadTrendData}}.}
\item{site.names}{character; an array of local site identifiers; if missing,
  all sites in the statistics configuration file are used.}
\item{is.censored}{logical; if \code{TRUE}, the data is assumed to be
  censorsed, its default is \code{FALSE}, see \sQuote{Details}.}
\item{initial.dir}{character; specifies the initial directory in the open files
  pop-up box.}
\item{file.parameters}{character; file path for the parameters
  configuration table, see \code{\link{ShowParameters}}.}
\item{file.stats}{character; file path for the statistics configuration table,
  see \sQuote{Format}.}
\item{file.out}{character; file path for the statistics summary table,
  see \sQuote{Value}.}
\item{figs.dir}{character; if \code{gr.type} not equal to \code{"windows"},
  the directory where statistic plots are saved.}
\item{avg.time}{character; the time interval into which the period of record
  is to be cut.}
\item{gr.type}{character; see \code{\link{OpenGraphicsDevice}}, its default
  is \code{"pdf"}.}
}


\format{
The imported statistics configuration table requires the following fields:
\code{"Site_id"}, a unique numerical identifier used to access site data;
\code{"Site_name"}, a local site identifier;
\code{"Parameters"}, comma seperated parameter names corresponding to
  header fields in the data table, see \code{\link{ReadTrendData}};
\code{"Start_date"}, something;
\code{"End_date"}, something; and
\code{"Remark"}, something.
}

\details{
Something
}

\value{
Something
}

\author{J.C. Fisher and L.C. Davis}

\examples{
f <- system.file("extdata/Example_Data.txt", package = "Trends")
d <- ReadTrendData(f)

file.parameters <- system.file("extdata/Example_Config_Parameters.txt",
                               package = "Trends")
file.stats <- system.file("extdata/Example_Stats_Uncensored.txt",
                          package = "Trends")

\donttest{RunTrendStats()}
}

\keyword{misc}
