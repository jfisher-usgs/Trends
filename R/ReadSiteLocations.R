ReadSiteLocations <- function(...) {
  obj <- readOGR(...)
  d <- obj@data[, 1:2]
  names(d) <- c("Site_id", "Site_name")
  d$Site_id <- as.numeric(d$Site_id)
  site.names <- strsplit(as.character(d$Site_name), " ")
  FUN <- function(i) paste(i[i != ""], collapse=" ")
  d$Site_name <- vapply(site.names, FUN, "")
  obj@data <- d
  return(obj)
}
