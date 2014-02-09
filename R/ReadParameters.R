ReadParameters <- function(file, draw.legend=FALSE) {

  # Read parameters configuration table
  obj <- read.table(file=file, header=TRUE, sep="\t", stringsAsFactors=FALSE,
                    comment.char="", row.names=1)

  if (draw.legend) {
    windows(width=5, height=10, pointsize=12)
    plot.new()
    lwd <- 0.5 * (96 / (6 * 12))
    legend(x="center", obj[, "Name"], pch=obj[, "pch"], col=obj[, "col"],
           pt.bg=obj[, "bg"], xpd=NA, bg="#FFFFFF", bty="o", box.lwd=lwd,
           pt.lwd=lwd, title="Parameters")
  }

  row.names(obj) <- make.names(row.names(obj))
  invisible(obj)
}
