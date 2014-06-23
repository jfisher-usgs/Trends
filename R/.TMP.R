


.ProcessRawData <- function(raw.data, parameters, detection.limits=NULL, 
                            date.fmt="%Y-%m-%d") {

  raw.data$Date <- as.Date(raw.data$Date, format=date.fmt)
  raw.data <- raw.data[!is.na(raw.data$Date), ]

  parameters$pch <- as.integer(parameters$pch)
  par.names <- make.names(parameters$Parameter)
  par.names <- sort(par.names[par.names %in% colnames(raw.data)])

  detection.limits$Date <- as.Date(detection.limits$Date, format=date.fmt)
  detection.limits[, -1] <- apply(detection.limits[, -1], 2, as.numeric)

  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(raw.data[[nam]]) & raw.data[[nam]] != ""
    if (all(!is.rec))
      next

    d <- raw.data[is.rec, c("Site_name", "Site_id", "Date")]
    d <- data.frame(d, code=NA, conc=NA, sd=NA, dl=NA, t1=NA, t2=NA,
                    is.event=NA, is.left=NA, is.interval=NA)
    d$Site_id   <- as.factor(d$Site_id)
    d$Site_name <- as.factor(d$Site_name)

    d$conc <- as.character(raw.data[is.rec, par.names[i]])
    d$code <- substr(d$conc, 1, 1)
    d$code[!d$code %in% c("<", "E", "V", "U")] <- ""
    d$code <- as.factor(d$code)
    is.code <- d$code != ""
    d$conc[is.code] <- substr(d$conc[is.code], 2, nchar(d$conc[is.code]))
    d$conc <- as.numeric(d$conc)
    d$conc[d$code %in% c("V", "U")] <- NA

    p <- parameters[match(nam, make.names(parameters$Parameter)), , drop=TRUE]
    sd.col <- p$sd
    idx <- ifelse(!is.na(sd.col), match(sd.col, colnames(raw.data)), NA)
    d$sd <- if (is.na(idx)) NA else as.numeric(raw.data[is.rec, idx])
    
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

    d$t2[which(d$t2 <= 0)] <- NA  # zero is invalid for 'lognormal' distribution
    d$t1[which(d$t1 <= 0 | is.na(d$t2))] <- NA

    is.t1 <- !is.na(d$t1)
    is.t2 <- !is.na(d$t2)
    d$is.event    <-  is.t1 & is.t2 & d$t1 == d$t2
    d$is.left     <- !is.t1 & is.t2
    d$is.interval <-  is.t1 & is.t2 & d$t1 != d$t2

    d <- d[order(d$Site_name, d$Date), ]
    attributes(d) <- c(attributes(d), p)

    lst[[p$Parameter]] <- d
  }

  return(lst)
}



.ProcessConfig <- function(config, processed.data) {

  ids <- unique(unlist(lapply(processed.data, function(i) levels(i$Site_id))))
  config <- config[config$Site_id %in% ids, ]

  FUN <- function(i) {
    d <- config[i, , drop=FALSE]
    p <- strsplit(d$Parameters, ",")[[1]]
    p <- unique(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", p, perl=TRUE))
    d <- data.frame(d, Parameter=p, rec=i, row.names=NULL, 
                    stringsAsFactors=FALSE)
    d$Parameters <- NULL
    return(d)
  }
  d <- do.call(rbind, lapply(seq_len(nrow(config)), FUN))
  
  d <- d[d$Parameter %in% names(processed.data), ]
  
  return(d)
}



.RunAnalysis <- function(processed.data, processed.config, sdate=NA, edate=NA, 
                         gr.type=c("pdf", "eps"), plot.path=NULL) {
  
  
  if (!is.na(sdate) && !inherits(sdate, "Date"))
    stop("incorrect class for argument 'sdate'")
  if (!is.na(edate) && !inherits(edate, "Date"))
    stop("incorrect class for argument 'edate'")
  
  
  
  models <- list()
  
  d <- processed.config[, c("Site_id", "Site_name", "Parameter")]
  stats <- data.frame(d, "min"=NA, "max"=NA, "median"=NA, "mean"=NA, "sd"=NA, 
                      "0.95LCL"=NA, "0.95UCL"=NA, check.names=FALSE)
  
  for (i in seq_len(nrow(processed.config))) {
    
    d <- processed.data[[processed.config[i, "Parameter"]]]
    d <- d[d$Site_id == processed.config[i, "Site_id"], ]
    
    d <- d[d$Date >= if(is.na(sdate)) min(d$Date) else sdate & 
           d$Date <= if(is.na(edate)) max(d$Date) else edate, ]
    
    d$surv <- Surv(time=d$t1, time2=d$t2, type="interval2")
    
    models[[i]] <- survreg(surv ~ Date, data=d, dist="lognormal")
    
    if (any(d$is.interval)) {
      fit <- summary(survfit(d$surv ~ 1))$table
      vars <- c("median", "0.95LCL", "0.95UCL")
      stats[i, vars] <- fit[vars]
      
    } else if (any(d$is.left)) {
      fit <- cenfit(d$t2, d$is.left)
      vars <- c("mean", "0.95LCL", "0.95UCL")
      stats[i, vars] <- mean(fit)[vars]
      stats[i, c("median", "sd")] <- c(median(fit), sd(fit))
      
    } else {
      vars <- c("min", "max", "median", "mean", "sd")
      stats[i, vars] <- c(min(d$t1), max(d$t1), median(d$t1), mean(d$t1), sd(d$t1))
    }
    
  }
  
  
  # x <- seq(sdate, edate, "days")
  # y <- predict(model, newdata=list(date=x), type="quantile", p=c(0.1, 0.9, 0.5))
    
  
  
}







.TMP <- function() {
  
  library(survival)
  library(NADA)


  path.in <- system.file("extdata", "SIR2014", package = "Trends")
  read.args <- list(header = TRUE, sep = "\t", colClasses = "character", na.strings = "", 
                    fill = TRUE, strip.white = TRUE, comment.char = "", flush = TRUE, 
                    stringsAsFactors = FALSE)

  file <- file.path(path.in, "Raw_Data.tsv")
  raw.data <- do.call(read.table, c(list(file), read.args))

  file <- file.path(path.in, "Parameters.tsv")
  parameters <- do.call(read.table, c(list(file), read.args))

  file <- file.path(path.in, "Detection_Limits.tsv")
  detection.limits <- do.call(read.table, c(list(file), read.args))

  processed.data <- .ProcessRawData(raw.data, parameters, detection.limits, 
                                    date.fmt = "%m/%d/%Y")
  
  ##
  
  file <- file.path(path.in, "Config_RADS.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  
  processed.config <- .ProcessConfig(config, processed.data)

  ##
  
  stats <- .RunAnalysis(processed.data, processed.config)
  
  
  
  
  

}



