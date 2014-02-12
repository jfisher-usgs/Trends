# Combine PDF files into a new file.
# Requires PDFtk Server (http://www.pdflabs.com/tools/pdftk-server/)

MergePDFs <- function(path, open.pdf=FALSE) {

  if (Sys.which("pdftk") == "")
    stop("pdftk can not be run, check that PDFtk Server is installed")

  if (missing(path) || !is.character(path))
    stop("argument 'path' is missing or not a character string")
  if (!is.logical(open.pdf))
    stop("argument 'open.pdf' is not logical")

  input.pdfs <- list.files(path, pattern=".pdf$")
  if (length(input.pdfs) == 0 || input.pdfs == "")
    stop("path does not exist or input pdf files are missing")

  dir.up <- sub(paste0("/", basename(path), "$"), "", path)
  output.pdf <- file.path(dir.up, paste0(basename(path), ".pdf"))
  if(file.exists(output.pdf))
    file.remove(output.pdf)

  tmp.txt <- tempfile(pattern="tmp", fileext=".txt")
  tmp.bat <- tempfile(pattern="tmp", fileext=".bat")
  tmp.pdf <- tempfile(pattern="tmp", fileext=".pdf")

  cmd <- paste("cd", shQuote(path))

  npages <- NULL
  for (i in input.pdfs) {
    cmd[2] <- paste("pdftk", shQuote(i), "dump_data output", shQuote(tmp.txt))
    cat(cmd, file=tmp.bat, sep="\n")
    system(command=tmp.bat, show.output.on.console=FALSE)
    txt <- scan(tmp.txt, what=character(), quiet=TRUE)
    npages <- c(npages, as.integer(txt[which(txt == "NumberOfPages:") + 1L]))
  }
  pages <- cumsum(npages) - (npages - 1L)

  cmd[2] <- paste("pdftk *.pdf cat output", shQuote(tmp.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "dump_data output",
                  shQuote(tmp.txt))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  bookmarks <- sub("\\.pdf$", "", input.pdfs)
  Fun <- function(i) {
    bookmark <- c("BookmarkBegin", paste("BookmarkTitle:", bookmarks[i]),
                  "BookmarkLevel: 1", paste("BookmarkPageNumber:", pages[i]))
    return(paste(bookmark, collapse="\n"))
  }
  bookmarks <- vapply(seq_along(bookmarks), Fun, "")
  cat(bookmarks, file=tmp.txt, sep="\n", append=TRUE)
  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "update_info", shQuote(tmp.txt),
                  "output", shQuote(output.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  if (open.pdf)
    system(paste("open", shQuote(output.pdf)), wait=FALSE,
           show.output.on.console=FALSE)

  unlink(tmp.txt)
  unlink(tmp.bat)
  unlink(tmp.pdf)

  invisible(output.pdf)
}
