\name{OpenGraphicsDevice}

\alias{OpenGraphicsDevice}

\title{Open Graphics Device}

\description{
Starts the device driver for producing graphics.
}

\usage{
OpenGraphicsDevice(gr.type, file.name, figs.dir,
                   w = 8.5, h = 11, p = 12, png.res = 300)
}

\arguments{
\item{gr.type}{character; the name of graphics driver, either
  \code{"pdf"}, \code{"postscript"}, \code{"png"}, or \code{"windows"}.}
\item{file.name}{character; the name of the graphics file without an extension.}
\item{figs.dir}{character; the directory where graphic files are saved.}
\item{w, h}{numeric; the (nominal) width and height of the canvas of the
  plotting window in inches. Default is \code{8.5} and \code{11}, respectively.}
\item{p}{numeric; the default pointsize of plotted text, its default is
  \code{12}.}
\item{png.res}{numeric; The nominal resolution in ppi which will be recorded
  in the PNG file. Default is \code{300} ppi to set the size of text and line
  widths.}
}

\details{
A \code{"postscript"} graphics device is recommended for figures of
publication quality.
}

\value{
A plot device is opened.
}

\author{Fisher, J.C.}

\seealso{
\code{\link{windows}}, \code{\link{pdf}}, \code{\link{png}},
\code{\link{postscript}}
}

\examples{
\donttest{OpenGraphicsDevice("pdf", "fig01", getwd())}
}

\keyword{misc}
