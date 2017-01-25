#' Merge PDF Files
#'
#' This function combines Portable Document Format (PDF) files into a single new PDF file.
#'
#' @param path character.
#'   Path name of the folder containing the PDF files to merge.
#' @param pdfs character.
#'   Vector of file names, if missing, all PDF files under \code{path} will be merged.
#' @param preserve.files logical.
#'   If true, all individual PDF files are preserved after a merge is completed.
#' @param open.file logical.
#'   If true, the merged PDF file is opened using your systems default PDF viewer.
#'
#' @details Names of the individual PDF files are used as bookmarks in the merged file.
#'   The merged file is placed one directory above the \code{path} folder.
#'
#' @return Returns the name of the merged file.
#'
#' @note Requires \href{https://www.pdflabs.com/tools/pdftk-server/}{PDFtk Server},
#'   a cross-platform command-line tool for working with PDFs.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{RunAnalysis}}, \code{\link{system}}
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' \donttest{
#'   # Create a temporary directory
#'   dir.create(path <- file.path(tempdir(), "merge"))
#'
#'   # Write three single-page PDF files to the temporary directory
#'   pdf(file.path(path, "f1.pdf"))
#'   plot(seq_len(10), main = "f1a")
#'   plot(sin, -pi, 2 * pi, main = "f1b")
#'   plot(qnorm, col = "red", main = "f1c")
#'   dev.off()
#'   pdf(file.path(path, "f2.pdf"))
#'   plot(table(rpois(100, 5)), type = "h", col = "yellow", main = "f2a")
#'   dev.off()
#'   pdf(file.path(path, "f3.pdf"))
#'   plot(x <- sort(rnorm(47)), type = "s", col = "green", main = "f3a")
#'   plot(x, main = "f3b")
#'   dev.off()
#'
#'   # Merge PDF files into a single file and open it in your default viewer
#'   MergePDFs(path, open.file = TRUE)
#'
#'   # Remove PDF files
#'   unlink(path, recursive = TRUE)
#' }
#'

MergePDFs <- function(path, pdfs, preserve.files=FALSE, open.file=FALSE) {

  if (Sys.which("pdftk") == "")
    stop("pdftk not found, check that PDFtk Server is installed")

  if (file.access(path) < 0)
    stop("path does not exist")

  if (missing(pdfs)) {
    pdfs <- list.files(path, pattern=".pdf$")
    if (length(pdfs) == 0 || pdfs == "")
      stop("no input PDF files were found in path")
  }

  if (any(file.access(file.path(path, pdfs)) < 0))
    stop("one or more input PDF files is missing")

  out.pdf <- file.path(dirname(path), paste0(basename(path), ".pdf"))
  if(file.exists(out.pdf))
    file.remove(out.pdf)

  tmp.txt <- tempfile(fileext=".txt")
  tmp.bat <- tempfile(fileext=".bat")
  tmp.pdf <- tempfile(fileext=".pdf")

  cmd <- paste("cd", shQuote(path))

  cat("", file=tmp.bat)
  Sys.chmod(tmp.bat, mode="755")

  npages <- NULL
  for (i in pdfs) {
    cmd[2] <- paste("pdftk", shQuote(i), "dump_data output", shQuote(tmp.txt))
    cat(cmd, file=tmp.bat, sep="\n")
    system(command=tmp.bat, show.output.on.console=FALSE)
    txt <- scan(tmp.txt, what=character(), quiet=TRUE)
    npages <- c(npages, as.integer(txt[which(txt == "NumberOfPages:") + 1L]))
  }
  pages <- cumsum(npages) - (npages - 1L)

  input.pdf.files <- paste(shQuote(pdfs), collapse=" ")
  cmd[2] <- paste("pdftk", input.pdf.files, "cat output", shQuote(tmp.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "dump_data output",
                  shQuote(tmp.txt))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  bookmarks <- sub("\\.pdf$", "", pdfs)
  FUN <- function(i) {
    bookmark <- c("BookmarkBegin", paste("BookmarkTitle:", bookmarks[i]),
                  "BookmarkLevel: 1", paste("BookmarkPageNumber:", pages[i]))
    return(paste(bookmark, collapse="\n"))
  }
  bookmarks <- vapply(seq_along(bookmarks), FUN, "")
  cat(bookmarks, file=tmp.txt, sep="\n", append=TRUE)
  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "update_info", shQuote(tmp.txt),
                  "output", shQuote(out.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  unlink(tmp.txt)
  unlink(tmp.bat)
  unlink(tmp.pdf)
  if (!preserve.files) {
    unlink(file.path(path, pdfs))
    if (length(list.files(path)) == 0)
      unlink(path, recursive=TRUE, force=TRUE)
  }

  if (open.file) {
    cmd <- paste("open", shQuote(out.pdf))
    system(cmd, show.output.on.console=FALSE, wait=FALSE)
  }

  invisible(out.pdf)
}
