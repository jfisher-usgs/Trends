OpenGraphicsDevice <- function(figs.dir, file.name, gr.type,
                               w=8.5, h=11, p=12, png.res=300) {
# This function opens a graphics device of type:
# "windows", "pdf", "png", or "postscript"

  if (gr.type == "windows") {
    windows(width=w, height=h, pointsize=p)
  } else {
    if (gr.type == "pdf") {
      f <- file.path(figs.dir, paste(file.name, "pdf", sep="."))
      pdf(file=f, width=w, height=h, pointsize=p, version="1.6",
          colormodel="cmyk")
    } else if (gr.type == "png") {
      f <- file.path(figs.dir, paste(file.name, "png", sep="."))
      png(filename=f, width=w, height=h, units="in", pointsize=p, res=png.res)

    } else if (gr.type == "postscript") {
      f <- file.path(figs.dir, paste(file.name, "eps", sep="."))
      postscript(file=f, width=w, height=h, pointsize=p,
                 horizontal=FALSE, paper="letter")
    } else {
      stop(paste("Graphics device", gr.type, "not recognized"))
    }
  }
}
