\name{OpenDevice}

\alias{OpenDevice}

\title{Open Graphics Device}

\description{This function starts the device driver for producing graphics.}

\usage{
OpenDevice(path.out, file.name, gr.type, w = 8.5, h = 11, p = 12, png.res = 300)
}

\arguments{
\item{path.out}{character; the directory where graphic files are saved.}
\item{file.name}{character; the name of the graphics file and excluding
  a file extension.}
\item{gr.type}{character; the name of graphics driver, either
  \code{"pdf"}, \code{"postscript"}, \code{"png"}, or \code{"windows"} (normal
  screen device).}
\item{w, h}{numeric; the (nominal) width and height of the canvas of the
  plotting window in inches. Default is \code{8.5} and \code{11}, respectively.}
\item{p}{numeric; the default point size of plotted text, its default is
  \code{12}.}
\item{png.res}{numeric; the nominal resolution in points per inch (ppi)
  which will be recorded in the
  \href{http://en.wikipedia.org/wiki/Portable_Network_Graphics}{PNG} file, its
  default is \code{300} ppi to set the size of text and line widths.}
}

\details{
A \code{"postscript"} graphics device produces an
\href{http://en.wikipedia.org/wiki/Encapsulated_PostScript}{EPS} file and is
recommended for producing figures of publication quality.
}

\value{A plot device is opened.}

\author{J.C. Fisher}

\seealso{
\code{\link{pdf}}, \code{\link{png}}, \code{\link{postscript}}
}

\examples{\donttest{OpenDevice(getwd(), "fig01", "pdf")}}

\keyword{misc}
