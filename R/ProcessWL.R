ProcessWL <- function(water.levels, date.fmt="%Y-%m-%d %H:%M") {

  d <- water.levels

  d$Date <- as.Date(as.POSIXct(d$LEV_DT, format=date.fmt))
  d <- d[!duplicated(paste(d$SITE_NO, d$Date)), ]

  d$Var <- suppressWarnings(as.numeric(d$ALT_VA) - as.numeric(d$LEV_VA))
  d <- d[!is.na(d$Var), ]

  d$Site_id <- as.factor(d$SITE_NO)

  d <- d[, c("Site_id", "Date", "Var")]

  return(d)
}
