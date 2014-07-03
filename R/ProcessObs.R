ProcessObs <- function(observations, parameters, detection.limits=NULL,
                       date.fmt="%Y-%m-%d") {

  observations$Date <- as.Date(observations$Date, format=date.fmt)
  observations <- observations[!is.na(observations$Date), ]

  par.names <- make.names(parameters$Parameter)
  par.names <- sort(par.names[par.names %in% colnames(observations)])

  detection.limits$Date <- as.Date(detection.limits$Date, format=date.fmt)
  detection.limits[, -1] <- apply(detection.limits[, -1], 2, as.numeric)

  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(observations[[nam]]) & observations[[nam]] != ""
    if (all(!is.rec))
      next

    d <- observations[is.rec, c("Site_name", "Site_id", "Date")]
    d <- data.frame(d, code=NA, conc=NA, sd=NA, dl=NA, t1=NA, t2=NA,
                    is.exact=NA, is.left=NA, is.interval=NA)
    d$Site_id   <- as.factor(d$Site_id)
    d$Site_name <- as.factor(d$Site_name)

    d$conc <- as.character(observations[is.rec, par.names[i]])
    d$code <- substr(d$conc, 1, 1)
    d$code[!d$code %in% c("<", "E", "V", "U")] <- ""
    d$code <- as.factor(d$code)
    is.code <- d$code != ""
    d$conc[is.code] <- substr(d$conc[is.code], 2, nchar(d$conc[is.code]))
    d$conc <- as.numeric(d$conc)
    d$conc[d$code %in% c("V", "U")] <- NA

    p <- parameters[match(nam, make.names(parameters$Parameter)), , drop=TRUE]
    sd.col <- p$sd
    idx <- ifelse(!is.na(sd.col), match(sd.col, colnames(observations)), NA)
    d$sd <- if (is.na(idx)) NA else as.numeric(observations[is.rec, idx])

    if (nam %in% colnames(detection.limits)) {
      dl <- detection.limits[!is.na(detection.limits[[nam]]), c("Date", nam)]
      for (id in levels(d$Site_id)) {
        idxs <- which(d$Site_id == id)
        breaks <- findInterval(as.numeric(dl$Date), as.numeric(d$Date[idxs]))
        breaks <- unique(c(breaks, length(idxs) + 1L))
        d$dl[idxs] <- dl[as.integer(cut(seq_along(idxs), breaks)), nam]
      }
    }

    is.left <- d$code %in% "<"
    is.exact <- !is.left & (is.na(d$sd) | is.na(d$dl))
    d$t1[is.exact] <- d$conc[is.exact]
    d$t2[is.exact] <- d$conc[is.exact]
    d$t2[is.left]  <- d$conc[is.left]
    lower.conc <- d$conc - 3 * d$sd
    upper.conc <- d$conc + 3 * d$sd

    are.left <- which(!is.exact & upper.conc <= d$dl)
    d$t2[are.left] <- d$dl[are.left]
    are.left <- which(!is.exact & lower.conc <= d$dl & upper.conc > d$dl)
    d$t2[are.left] <- upper.conc[are.left]

    are.interval <- which(!is.exact & lower.conc > d$dl)
    d$t1[are.interval] <- lower.conc[are.interval]
    d$t2[are.interval] <- upper.conc[are.interval]

    d$t2[which(d$t2 <= 0)] <- NA  # TODO: zero is invalid for 'lognormal' distribution
    d$t1[which(d$t1 <= 0 | is.na(d$t2))] <- NA

    is.t1 <- !is.na(d$t1)
    is.t2 <- !is.na(d$t2)
    d$is.exact    <-  is.t1 & is.t2 & d$t1 == d$t2
    d$is.left     <- !is.t1 & is.t2
    d$is.interval <-  is.t1 & is.t2 & d$t1 != d$t2

    d <- d[order(d$Site_name, d$Date), ]
    attributes(d) <- c(attributes(d), p[c("Parameter", "Name", "Units")])

    lst[[p$Parameter]] <- d
  }

  return(lst)
}
