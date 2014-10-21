ProcessWL <- function(water.levels, date.fmt="%Y-%m-%d %H:%M") {

  d$Date <- as.Date(as.POSIXct(d$LEV_DT, format=date.fmt))
  d <- d[!duplicated(paste(d$SITE_NO, d$Date)), ]

  d$WL <- d$ALT_VA - d$LEV_VA
  d <- d[!is.na(d$WL), ]

  d$Site_id <- as.factor(d$SITE_NO)

  d <- d[, c("Site_id", "Date", "WL")]

  return(d)
}
