ReadParConfig <- function(file, sep="\t", draw.legend=FALSE) {

  col.names <- c("Parameter", "Name", "Units", "pch", "col", "bg")
  col.classes <- c("character", "character", "factor", "integer", "character")
  obj <- read.table(file=file, header=TRUE, sep=sep, col.names=col.names,
                    colClasses=col.classes, flush=TRUE, fill=TRUE,
                    stringsAsFactors=FALSE, comment.char="", row.names=1)

  if (draw.legend) {
    dev.new(width=5, height=10, pointsize=12)
    plot.new()
    lwd <- 0.5 * (96 / (6 * 12))
    legend(x="center", obj$Name, pch=obj$pch, col=obj$col, pt.bg=obj$bg, xpd=NA,
           bg="#FFFFFF", bty="o", box.lwd=lwd, pt.lwd=lwd, title="Parameters")
  }

  row.names(obj) <- make.names(row.names(obj))
  invisible(obj)
}
