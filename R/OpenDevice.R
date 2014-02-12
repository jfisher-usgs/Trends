# Open a graphics device of type "windows", "pdf", "png", or "postscript"

OpenDevice <- function(path.out, file.name, gr.type, w=8.5, h=11, p=12,
                       png.res=300) {
  if (gr.type == "windows") {
    dev.new(width=w, height=h, pointsize=p)
  } else {
    if (gr.type == "postscript") {
      file.ext <- "eps"
    } else if (gr.type %in% c("pdf", "png")) {
      file.ext <- gr.type
    } else {
      stop(paste("Graphics device", gr.type, "not recognized"))
    }
    f <- file.path(path.out, paste(file.name, file.ext, sep="."))
    if (file.access(f, mode=0) == 0)
      stop(paste(f, "already exists and will not be overwritten"))
    if (gr.type == "pdf") {
      pdf(file=f, width=w, height=h, pointsize=p, version="1.6",
          colormodel="cmyk")
    } else if (gr.type == "png") {
      png(filename=f, width=w, height=h, units="in", pointsize=p, res=png.res)
    } else if (gr.type == "postscript") {
      postscript(file=f, width=w, height=h, pointsize=p,
                 horizontal=FALSE, paper="letter")
    }
  }
  par(mfrow=c(4, 1), oma=c(5, 5, 5, 5), mar=c(2, 5, 2, 2))
}
