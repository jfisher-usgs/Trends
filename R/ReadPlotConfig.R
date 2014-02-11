ReadPlotConfig <- function(file, sep="\t") {
  col.names <- c("Site_id", "Site_name", "Parameters", "Axis_title")
  col.classes <- c("numeric", "character", "character", "factor")
  obj <- read.table(file=file, header=TRUE, sep=sep, col.names=col.names,
                    colClasses=col.classes, flush=TRUE, fill=TRUE,
                    stringsAsFactors=FALSE)
  return(obj)
}
