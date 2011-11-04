ShowSymbols <- function(file.symbs) {

  # Read symbol configuration table
  tbl.sym <- read.table(file=file.symbs, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE, comment.char="", row.names=1)

  windows(width=5, height=10, pointsize=12)
  plot.new()

  lwd <- 0.5 * (96 / (6 * 12))

  legend(x="center", tbl.sym[, "Name"], pch=tbl.sym[, "pch"],
         col=tbl.sym[, "col"], pt.bg=tbl.sym[, "bg"],
         xpd=NA, bg="#FFFFFF", bty="o", box.lwd=lwd, pt.lwd=lwd,
         title="Example")
}
