

.RunAnalysis <- function(d, site.id, sdate=NA, edate=NA) {
  
  
  
}


.ProcessRawData <- function(raw.data, parameters, detection.limits=NULL, 
                            date.fmt="%Y-%m-%d") {

  parameters$Parameter <- make.names(parameters$Parameter)
  par.names <- parameters$Parameter
  par.names <- sort(par.names[par.names %in% colnames(raw.data)])

  raw.data$Site_id <- as.factor(raw.data$Site_id)
  raw.data$Site_name <- as.factor(raw.data$Site_name)
  raw.data$Date <- as.Date(raw.data$Date, format=date.fmt)
  raw.data <- raw.data[!is.na(raw.data$Date), ]

  detection.limits$Date <- as.Date(detection.limits$Date, format=date.fmt)

  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(raw.data[[nam]]) & raw.data[[nam]] != ""
    if (all(!is.rec))
      next

    d <- raw.data[is.rec, c("Site_name", "Site_id", "Date")]
    d <- data.frame(d, code=NA, conc=NA, sd=NA, dl=NA, t1=NA, t2=NA,
                    is.event=NA, is.left=NA, is.interval=NA)

    d$conc <- as.character(raw.data[is.rec, par.names[i]])
    d$code <- substr(d$conc, 1, 1)
    d$code[!d$code %in% c("<", "E", "V", "U")] <- ""
    d$code <- as.factor(d$code)
    is.code <- d$code != ""
    d$conc[is.code] <- substr(d$conc[is.code], 2, nchar(d$conc[is.code]))
    d$conc <- as.numeric(d$conc)
    d$conc[d$code %in% c("V", "U")] <- NA

    col.name <- parameters$sd[parameters$Parameter %in% nam]
    idx <- ifelse(!is.na(col.name), match(col.name, colnames(raw.data)), NA)
    d$sd <- if (is.na(idx)) NA else as.numeric(raw.data[is.rec, idx])
    
    if (nam %in% colnames(detection.limits)) {
      dl <- detection.limits[!is.na(detection.limits[[nam]]), c("Date", nam)]
      for (id in unique(d$Site_id)) {
        idxs <- which(d$Site_id == id)
        breaks <- findInterval(as.numeric(dl$Date), as.numeric(d$Date[idxs]))
        breaks <- unique(c(breaks, length(idxs) + 1L))
        d$dl[idxs] <- dl[as.integer(cut(seq_along(idxs), breaks)), nam]
      }
    }

    is.left <- d$code %in% "<"
    is.event <- !is.left & (is.na(d$sd) | is.na(d$dl))
    d$t1[is.event] <- d$conc[is.event]
    d$t2[is.event] <- d$conc[is.event]
    d$t2[is.left]  <- d$conc[is.left]
    lower.conc <- d$conc - 3 * d$sd
    upper.conc <- d$conc + 3 * d$sd

    are.left <- which(!is.event & upper.conc <= d$dl)
    d$t2[are.left] <- d$dl[are.left]
    are.left <- which(!is.event & lower.conc <= d$dl & upper.conc > d$dl)
    d$t2[are.left] <- upper.conc[are.left]

    are.interval <- which(!is.event & lower.conc > d$dl)
    d$t1[are.interval] <- lower.conc[are.interval]
    d$t2[are.interval] <- upper.conc[are.interval]

    d$t2[which(d$t2 < 0)] <- 0
    d$t1[which(d$t1 < 0 | d$t2 == 0)] <- 0

    is.t1 <- !is.na(d$t1)
    is.t2 <- !is.na(d$t2)
    d$is.event    <-  is.t1 & is.t2 & d$t1 == d$t2
    d$is.left     <- !is.t1 & is.t2
    d$is.interval <-  is.t1 & is.t2 & d$t1 != d$t2

    d <- d[order(d$Site_name, d$Date), ]

    attrs <- parameters[match(nam, parameters$Parameter), , drop=TRUE]
    attributes(d) <- c(attributes(d), attrs)

    lst[[nam]] <- d
  }

  return(lst)
}




.TMP <- function() {


  path.in <- system.file("extdata", "SIR2014", package = "Trends")
  read.args <- list(header = TRUE, sep = "\t", na.strings = "", fill = TRUE, 
                    strip.white = TRUE, comment.char = "", flush = TRUE, 
                    stringsAsFactors = FALSE)

  file <- file.path(path.in, "Raw_Data.tsv")
  raw.data <- do.call(read.table, c(list(file, colClasses = "character"), read.args))

  file <- file.path(path.in, "Parameters.tsv")
  parameters <- do.call(read.table, c(list(file), read.args))

  file <- file.path(path.in, "Detection_Limits.tsv")
  detection.limits <- do.call(read.table, c(list(file), read.args))

  processed.data <- .ProcessRawData(raw.data, parameters, detection.limits, 
                                    date.fmt = "%m/%d/%Y")


}

