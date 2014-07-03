\name{MergePDFs}

\alias{MergePDFs}

\title{Merge PDF Files}

\description{This function combines Portable Document Format (PDF) files into a new file.}

\usage{MergePDFs(path, preserve.files = FALSE, open.file = FALSE)}

\arguments{
\item{path}{character; the path name of the folder containing the PDF files to merge.}
\item{preserve.files}{logical; if \code{TRUE}, all individual PDF files are preserved after a merge is completed.}
\item{open.file}{logical; if \code{TRUE}, the merged PDF file is opened using your systems default PDF viewer.}
}

\details{
Names of the individual PDF files are used as bookmarks in the merged file.
The merged file is placed one directory above \code{path}.
}

\value{The name of the merged file is returned.}

\note{
Requires \href{http://www.pdflabs.com/tools/pdftk-server/}{PDFtk Server}, a cross-platform command-line tool for working with PDFs.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}, \code{\link{system}}}

\examples{\donttest{
path <- file.path(tempdir(), "merge")
dir.create(path)

f1 <- file.path(path, "f1.pdf")
f2 <- file.path(path, "f2.pdf")
f3 <- file.path(path, "f3.pdf")

pdf(f1)
plot(seq_len(10), main = "f1a")
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

MergePDFs(path, open.file = TRUE)

unlink(paste0(path, ".pdf"))
}}

\keyword{misc}
