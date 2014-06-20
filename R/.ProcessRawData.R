


.ProcessRawData <- function(raw.data, par.config, det.lim=NULL) {

  par.config$Parameter <- make.names(par.config$Parameter)
  par.names <- par.config$Parameter
  par.names <- sort(par.names[par.names %in% colnames(raw.data)])

  raw.data$Site_id <- as.factor(raw.data$Site_id)
  raw.data$Dates <- as.Date(raw.data$Dates, format="%m/%d/%Y")
  raw.data <- raw.data[!is.na(raw.data$Dates), ]


  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(raw.data[[nam]]) & raw.data[[nam]] != ""
    if (all(!is.rec))
      next

    d <- raw.data[is.rec, c("Site_id", "Dates")]
    d <- data.frame(d, code=NA, conc=NA, sd=NA, dl=NA, t1=NA, t2=NA,
                    is.event=NA, is.left=NA, is.interval=NA)

    d$conc <- as.character(raw.data[is.rec, par.names[i]])
    d$code <- match(substr(d$conc, 1, 1), c("<", "E", "V", "U"), nomatch=0L)
    is.coded <- d$code > 0L
    d$conc[is.coded] <- substr(d$conc[is.coded], 2, nchar(d$conc[is.coded]))
    d$conc <- as.numeric(d$conc)

    d$sd <- as.numeric(NA)  # TODO
    d$dl <- as.numeric(NA)  # TODO

    is.event <- d$code != 1L & (is.na(d$sd) | is.na(d$dl))
    d$t1[is.event] <- d$conc[is.event]
    d$t2[is.event] <- d$conc[is.event]

    is.left <- d$code == 1L
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
    d$is.interval <- !is.na(d$t1) & !is.na(d$t2) & d$t1 < d$t2

    att <- par.config[match(nam, par.config$Parameter), , drop=TRUE]
    attr(d, "Name")  <- att$Name
    attr(d, "Units") <- att$Units
    attr(d, "pch")   <- att$pch
    attr(d, "col")   <- att$col
    attr(d, "bg")    <- att$bg
    lst[[nam]] <- d
  }
  return(lst)
}




.TMP <- function() {


  path.in <- system.file("extdata", "SIR2014", package = "Trends")

  file <- file.path(path.in, "Data.tsv")
  raw.data <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, strip.white = TRUE,
                         allowEscapes = TRUE, flush = TRUE, stringsAsFactors = FALSE)


  file <- file.path(path.in, "Config_Par.tsv")
  par.config <- read.table(file, header = TRUE, sep = "\t", fill = TRUE, comment.char = "",
                           flush = TRUE, stringsAsFactors = FALSE)

  processed.data <- .ProcessRawData(raw.data, par.config)


}



