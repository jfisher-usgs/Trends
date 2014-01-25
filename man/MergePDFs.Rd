\name{MergePDFs}

\alias{MergePDFs}

\title{Merge PDF Files}

\description{
This function combines PDF files into a new file.
}

\usage{
MergePDFs(path, open.pdf = FALSE)
}

\arguments{
\item{path}{character; the path name of the folder containing the PDF files to
  merge.}
\item{open.pdf}{logical; if \code{TRUE}, the merged PDF file is opened using the
  systems default PDF viewer.}
}

\details{
Names of the individual PDF files are used as bookmarks in the merged PDF file.
}

\value{
The name of the merged file is returned.
}

\author{J.C. Fisher}

\examples{\donttest{
path <- file.path(tempdir(), "merge")
dir.create(path)

f1 <- file.path(path, "f1.pdf")
f2 <- file.path(path, "f2.pdf")
f3 <- file.path(path, "f3.pdf")

pdf(f1)
plot(1:10, main = "f1a")
plot(sin, -pi, 2 * pi, main = "f1b")
plot(qnorm, col = "red", main = "f1c")
dev.off()

pdf(f2)
plot(table(rpois(100, 5)), type = "h", col = "yellow", main = "f2a")
dev.off()

pdf(f3)
plot(x <- sort(rnorm(47)), type = "s", col = "green", main = "f3a")
plot(x, main = "f3b")
dev.off()

MergePDFs(path, open.pdf = TRUE)

unlink(path, recursive = TRUE)
unlink(paste0(path, ".pdf"))
}}

\keyword{misc}
