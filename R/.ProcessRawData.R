


.ProcessRawData <- function(raw.data, parameters, det.lim=NULL) {

  parameters$Parameter <- make.names(parameters$Parameter)
  par.names <- parameters$Parameter
  par.names <- sort(par.names[par.names %in% colnames(raw.data)])

  raw.data$Site_id <- as.factor(raw.data$Site_id)
  raw.data$Site_name <- as.factor(raw.data$Site_name)
  raw.data$Date <- as.Date(raw.data$Date, format="%m/%d/%Y")
  raw.data <- raw.data[!is.na(raw.data$Date), ]

  det.lim$Date <- as.Date(det.lim$Date, format="%m/%d/%Y")

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
    is.coded <- d$code != ""
    d$conc[is.coded] <- substr(d$conc[is.coded], 2, nchar(d$conc[is.coded]))
    d$conc <- as.numeric(d$conc)

    d$conc[d$code %in% c("V", "U")] <- NA

    d$sd <- as.numeric(NA)  # TODO
    d$dl <- as.numeric(NA)  # TODO

    is.event <- !d$code %in% "<" & (is.na(d$sd) | is.na(d$dl))
    d$t1[is.event] <- d$conc[is.event]
    d$t2[is.event] <- d$conc[is.event]

    is.left <- d$code %in% "<"
    d$t2[is.left] <- d$conc[is.left]
    lower.conc <- d$conc - 3 * d$sd
    upper.conc <- d$conc + 3 * d$sd

    left <- which(!is.event & upper.conc <= d$dl)
    d$t2[left] <- d$dl[left]
    left <- which(!is.event & lower.conc <= d$dl & upper.conc > d$dl)
    d$t2[left] <- upper.conc[left]
    interval <- which(!is.event & lower.conc > d$dl)
    d$t1[interval] <- lower.conc[interval]
    d$t2[interval] <- upper.conc[interval]

    d$t2[which(d$t2 < 0)] <- 0
    d$t1[which(d$t1 < 0 | d$t2 == 0)] <- 0

    d$is.event    <- !is.na(d$t1) & !is.na(d$t2) & d$t1 == d$t2
    d$is.left     <-  is.na(d$t1) & !is.na(d$t2)
    d$is.interval <- !is.na(d$t1) & !is.na(d$t2) & d$t1 != d$t2

    d <- d[order(d$Site_name, d$Date), ]

    added.attr <- parameters[match(nam, parameters$Parameter), , drop=TRUE]
    attributes(d) <- c(attributes(d), added.attr)

    lst[[nam]] <- d
  }

  return(lst)
}




.TMP <- function() {


  path.in <- system.file("extdata", "SIR2014", package = "Trends")

  file <- file.path(path.in, "Raw_Data.tsv")
  raw.data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, strip.white = TRUE,
                         allowEscapes = TRUE, flush = TRUE, stringsAsFactors = FALSE)


  file <- file.path(path.in, "Parameters.tsv")
  parameters <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "",
                           flush = TRUE, stringsAsFactors = FALSE)

  file <- file.path(path.in, "Detection_Limits.tsv")
  det.lim <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, flush = TRUE,
                        stringsAsFactors = FALSE)

  processed.data <- .ProcessRawData(raw.data, parameters, det.lim)


}



