ShowParameters <- function(file.parameters) {

  # Read parameters configuration table
  tbl.par <- read.table(file=file.parameters, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE, comment.char="", row.names=1)

  windows(width=5, height=10, pointsize=12)
  plot.new()

  lwd <- 0.5 * (96 / (6 * 12))

  legend(x="center", tbl.par[, "Name"], pch=tbl.par[, "pch"],
         col=tbl.par[, "col"], pt.bg=tbl.par[, "bg"],
         xpd=NA, bg="#FFFFFF", bty="o", box.lwd=lwd, pt.lwd=lwd,
         title="Parameters")
}
