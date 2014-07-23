# Combine PDF files into a new file.
# Requires PDFtk Server (http://www.pdflabs.com/tools/pdftk-server/)

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
