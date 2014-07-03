ProcessObs <- function(observations, parameters, detection.limits=NULL,
                       date.fmt="%Y-%m-%d") {

  observations$Date <- as.Date(observations$Date, format=date.fmt)
  observations <- observations[!is.na(observations$Date), ]

  par.names <- make.names(parameters$Parameter_id)
  par.names <- sort(par.names[par.names %in% colnames(observations)])

  detection.limits$Date <- as.Date(detection.limits$Date, format=date.fmt)
  detection.limits[, -1] <- apply(detection.limits[, -1], 2, as.numeric)

  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(observations[[nam]]) & observations[[nam]] != ""
    if (all(!is.rec))
      next

    d <- observations[is.rec, c("Site_id", "Site_name", "Date")]
    d <- data.frame(d, code=NA, value=NA, sd=NA, dl=NA)
    d$Site_id   <- as.factor(d$Site_id)
    d$Site_name <- as.factor(d$Site_name)

    d$value <- as.character(observations[is.rec, par.names[i]])
    d$code <- substr(d$value, 1, 1)
    d$code[!d$code %in% c("<", "E", "V", "U")] <- ""
    d$code <- as.factor(d$code)
    is.code <- d$code != ""
    d$value[is.code] <- substr(d$value[is.code], 2, nchar(d$value[is.code]))
    d$value <- as.numeric(d$value)
    d$value[d$code %in% c("V", "U")] <- NA

    p <- parameters[match(nam, make.names(parameters$Parameter_id)), , drop=TRUE]
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

    t1 <- t2 <- rep(NA, nrow(d))

    is.left <- d$code %in% "<"
    is.exact <- !is.left & (is.na(d$sd) | is.na(d$dl))
    t1[is.exact] <- d$value[is.exact]
    t2[is.exact] <- d$value[is.exact]
    t2[is.left]  <- d$value[is.left]
    lower.value <- d$value - 3 * d$sd
    upper.value <- d$value + 3 * d$sd

    are.left <- which(!is.exact & upper.value <= d$dl)
    t2[are.left] <- d$dl[are.left]
    are.left <- which(!is.exact & lower.value <= d$dl & upper.value > d$dl)
    t2[are.left] <- upper.value[are.left]

    are.interval <- which(!is.exact & lower.value > d$dl)
    t1[are.interval] <- lower.value[are.interval]
    t2[are.interval] <- upper.value[are.interval]

    t2[which(t2 <= 0)] <- NA  # TODO: zero is invalid for 'lognormal' distribution
    t1[which(t1 <= 0 | is.na(t2))] <- NA

    d$surv <- Surv(time=t1, time2=t2, type="interval2")

    d <- d[order(d$Site_name, d$Date), ]
    vars <- c("Parameter_id", "Parameter_name", "Units")
    attributes(d) <- c(attributes(d), p[vars])

    lst[[p$Parameter_id]] <- d
  }

  return(lst)
}
