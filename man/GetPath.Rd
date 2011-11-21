\name{GetPath}

\alias{GetPath}

\title{Get Path to File or Directory}

\description{
This function is used to select a file or directory path.
}

\usage{
GetPath(type, path = NULL, initial.dir = getwd())
}

\arguments{
\item{type}{character; code used to identify the path type,
  see \sQuote{Details}.}
\item{path}{character; a file or directory path. If \code{NULL}, a dialog
  pops up.}
\item{initial.dir}{character; specifies the initial directory that should
  be displayed when the dialog pops up. Its default is the working directory.}
}

\details{
Options for path \code{type} include:
\code{"input_data"}, a data input file;
\code{"config_para"}, a parameter configuration input file;
\code{"config_plot"}, a plots configuration input file;
\code{"config_stat"}, a statistics configuration input file;
\code{"output_stat"}, a statistics output file; and
\code{"output_figs"}, a output directory where graphic files are saved.
A check is made to confirm that the file or directory exists.
}

\author{J.C. Fisher}

\examples{
\donttest{f <- GetPath("input_data")
d <- GetPath("output_figs")}
}

\keyword{misc}
