

ReadPlotConfigObs <- function(file, sep="\t") {
  col.names <- c("Site_id", "Site_name", "Parameters", "Min", "Max",
                 "Axis_title")
  col.classes <- c("numeric", "factor", "character", "numeric", "numeric",
                   "factor")
  obj <- read.table(file=file, header=TRUE, sep=sep, col.names=col.names,
                    colClasses=col.classes, flush=TRUE, stringsAsFactors=FALSE)
  return(obj)
}


ReadPlotConfigTrends <- function(file, sep="\t") {
  col.names <- c("Site_id", "Site_name", "Parameters", "Start_date", "End_date")
  col.classes <- c("numeric", "factor", "character", "character", "character")
  obj <- read.table(file=file, header=TRUE, sep=sep, col.names=col.names,
                    colClasses=col.classes, flush=TRUE, stringsAsFactors=FALSE)
  obj$Start_date <- as.POSIXct(obj$Start_date, "%m/%d/%Y", tz="")
  obj$End_date <- as.POSIXct(obj$End_date, "%m/%d/%Y", tz="")
  return(obj)
}

