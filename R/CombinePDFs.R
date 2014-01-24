# Combine PDF files into a new file.
# Requires PDFtk Server http://www.pdflabs.com/tools/pdftk-server/

CombinePDFs <- function(path, open.pdf=FALSE) {
  if (.Platform$OS.type != "windows")
    stop("function requires a Windows platform")

  input.pdfs <- list.files(path, pattern=".pdf$")
  if (length(input.pdfs) == 0 || input.pdfs == "")
    stop("path does not exist or input pdf files are missing")

  dir.up <- sub(paste0("/", basename(path), "$"), "", path)
  output.pdf <- file.path(dir.up, paste0(basename(path), ".pdf"))
  if(file.exists(output.pdf))
    file.remove(output.pdf)

  tmp.pdf <- tempfile(pattern="tmp", tmpdir=tempdir(), fileext=".pdf")
  cmd <- NULL
  cmd[1] <- paste("CD /d", shQuote(path))
  cmd[2] <- paste("pdftk", paste(shQuote(input.pdfs), collapse=" "),
                  "cat output", shQuote(tmp.pdf))
  tmp.bat <- tempfile(pattern="tmp", tmpdir=tempdir(), fileext=".bat")
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  tmp.txt <- tempfile(pattern="tmp", tmpdir=tempdir(), fileext=".txt")
  cmd <- NULL
  cmd[1] <- paste("CD /d", shQuote(path))
  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "dump_data output",
                  shQuote(tmp.txt))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  bookmarks <- sub("\\.pdf$", "", input.pdfs)
  Fun <- function(i) {
    return(paste("BookmarkBegin", paste("BookmarkTitle:", bookmarks[i]),
                 "BookmarkLevel: 1", paste("BookmarkPageNumber:", i), sep="\n"))
  }
  txt <- vapply(seq_along(bookmarks), Fun, "")
  cat(txt, file=tmp.txt, sep="\n", append=TRUE)

  cmd <- NULL
  cmd[1] <- paste("CD /d", shQuote(path))
  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "update_info", shQuote(tmp.txt),
                  "output", shQuote(output.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  if (open.pdf)
    system(paste("open", shQuote(output.pdf)))

  invisible(output.pdf)
}
