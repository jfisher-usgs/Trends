RunStats <- function(d, site.names, is.censored=FALSE, initial.dir=getwd(),
                     file.stats=NULL, file.out=NULL, figs.dir=NULL,
                     avg.time="year", gr.type="pdf") {
# This function performs a statistical analysis on uncensored and censored data
# tbl <- RunStats(d, c("ANP 6", "ARBOR TEST"))

  # Additional functions (subroutines):

  # Read table data from file
  ReadTable <- function(f) {
    tbl <- read.table(file=f, header=TRUE, sep="\t", stringsAsFactors=FALSE,
                      fill=TRUE)
    tbl[, "Start_date"] <- as.POSIXct(tbl[, "Start_date"], "%m/%d/%Y", tz="MST",
                                      origin=origin)
    tbl[, "End_date"] <- as.POSIXct(tbl[, "End_date"], "%m/%d/%Y", tz="MST",
                                    origin=origin)
    tbl
  }

  # Trim leading and trailing white space from character string
  trim <- function(s) {
    sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", s, perl=TRUE)
  }

  # Calculate percentage change in slope and uncertainties, time limits in days
  # Need start and end times (t) in days to work out concentrations at those
  # points. Slope is given in concentration per year. Returns percent change in
  # percent per year.
  PercentChange <- function(slope, intercept, tlim) {
    t1 <- tlim[1]
    t2 <- tlim[2]
    c1 <- slope * (t1 / 365) + intercept
    c2 <- slope * (t2 / 365) + intercept
    (((c2 / c1) - 1) / (t2 - t1)) * 365 * 100
  }


  # Main program:

  require(tcltk)
  require(NADA)
  require(Kendall)

  # Statistics configuration file
  if (is.null(file.stats)) {
    txt <- "Open configuration file for statistics"
    file.stats <- paste(tcl("tk_getOpenFile", initialdir=initial.dir, title=txt,
                            filetypes="{{Text files} {.txt}} {{All files} {*}}",
                            multiple=FALSE), collapse=" ")
  }
  if (!file.exists(file.stats))
    stop(paste("Statistics file does not exist:", file.stats))

  # Output file
  if (is.null(file.out)) {
    txt <- paste("Choose file to write output table")
    file.out <- paste(tcl("tk_getSaveFile", initialdir=initial.dir, title=txt,
                          filetypes="{{Text files} {.txt}} {{All files} {*}}",
                          defaultextension="txt"), collapse=" ")
    if (length(file.out) == 0)
      stop("Output file is required to continue")
  }

  # Output folder for figure files
  if (gr.type != "windows" & is.null(figs.dir)) {
    require(tcltk)
    txt <- paste("Choose a directory to store", gr.type,
                 "files, then select OK")
    figs.dir <- paste(tcl("tk_chooseDirectory", initialdir=initial.dir,
                             title=txt), collapse=" ")
    if (length(figs.dir) == 0)
      stop("Output folder is required to continue")
  }

  # Column names in data table
  d.names <- names(d)

  # Read data from statistics tables and combine
  tbl <- read.table(file=file.stats, header=TRUE, sep="\t",
                    stringsAsFactors=FALSE, fill=TRUE)

  # Convert date-time fields into POSIXct class
  origin <- as.POSIXct("1920-01-01 00:00:00.0")
  tbl[, "Start_date"] <- as.POSIXct(tbl[, "Start_date"], "%m/%d/%Y", tz="MST",
                                    origin=origin)
  tbl[, "End_date"] <- as.POSIXct(tbl[, "End_date"], "%m/%d/%Y", tz="MST",
                                  origin=origin)

  # Output table
  tbl.out <- NULL

  # Identify row index numbers of table
  if (missing(site.names))
    idxs <- 1:nrow(tbl)
  else
    idxs <- which(tbl$Site_name %in% site.names)

  # Loop though records in statistic table

  for (i in seq(along=idxs)) {
    idx <- idxs[i]

    id     <- tbl[idx, "Site_ID"]
    site   <- tbl[idx, "Site_name"]
    sdate  <- tbl[idx, "Start_date"]
    edate  <- tbl[idx, "End_date"]
    remark <- tbl[idx, "Remark"]
    if (is.na(remark))
      remark <- ""

    const.names <- trim(unique(unlist(strsplit(tbl[idx, "Constituents"], ","))))
    consts <- make.names(const.names)

    # Loop through constituents in record

    for (j in seq(along=consts)) {
      const <- consts[j]
      if (!const %in% d.names) {
        txt <- paste("Constituent is not recognized and will be skipped:\n",
                     "Row index: ", idx, ", Column name: Constituents, ",
                     "String: ", const, "\n", sep="")
        warning(txt)
        next
      }

      # Start record that will be added to output table
      lst <- list("Site_name"=site, "Constituent"=const.names[j],
                  "Start_date"=sdate, "End_date"=edate)
      rec <- as.data.frame(lst, optional=TRUE)

      # Determine pertinent column names in data table
      col.names <- c("datetime", const)
      col.code.name <- paste(const, "_code", sep="")
      is.code <- col.code.name %in% d.names
      if (is.code)
        col.names <- c(col.names, col.code.name)

      # Reduce size of data table
      is.id <- d$Site_ID == id
      is.dt <- d$datetime >= sdate & d$datetime <= edate
      d.id <- d[is.id & is.dt, col.names]

      # Order dates
      d.id <- d.id[order(d.id$datetime), ]

      # Determine the number of samples that are below the recording limit
      if (is.code)
        below.rl <- sum(as.integer(d.id[[col.code.name]] %in% 1:2))
      else
        below.rl <- 0

      # Text to append to error messages
      err.extra <- paste("Row index: ", idx, ", Constituent:, ", const,
                         "\n", sep="")

      # Start statistical analysis

      if (is.censored) {

        print("notyet")

      } else {

        # Basic summary statistics
        dat <- na.omit(d.id[[const]])
        n <- length(dat)
        lst <- list("n"=n, "n.above.rl"=n - below.rl,
                    "mean"=mean(dat), "median"=median(dat),
                    "min"=min(dat), "max"=max(dat),
                    "std.dev"=sd(dat), "len_record"=edate - sdate,
                    "remark"=remark)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Determine date cuts based on time interval
        ans <- try(cut(d.id$datetime, avg.time), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Time cut error:", ans, err.extra, sep="\n"))
          next
        } else {
          d.id.cuts <- as.POSIXct(ans, "%Y-%m-%d", tz="MST", origin=origin)
        }

        # Time average constituent based on date cuts
        ans <- try(aggregate(d.id, list(date=d.id.cuts),
                             function(i) mean(i, na.rm=TRUE)), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Time average error:", ans, err.extra, sep="\n"))
          next
        } else {
          d.id.time <- na.omit(ans[, c("date", const)])
        }

        # Theil Sen

     ## require(openair)
     ## ans <- try(TheilSen(d.id.time, const, avg.time="year",
     ##            slope.percent=TRUE), silent=TRUE)
     ## if (inherits(ans, "try-error") | is.null(ans)) {
     ##   warning(paste("TheilSen error:", err.extra, sep="\n"))
     ##   next
     ## }
     ## ts.cols <- c("p", "slope", "lower", "upper",
     ##              "intercept", "intercept.lower", "intercept.upper",
     ##              "slope.percent", "lower.percent", "upper.percent")
     ## rec <- cbind(rec, ans$data$res2[1, ts.cols])

        est <- try(regci(as.numeric(d.id.time$date), d.id.time[, const],
                         alpha=0.5, pr=FALSE)$regci, silent=TRUE)
        if (inherits(est, "try-error") | is.null(est)) {
          warning(paste("regci error:", err.extra, sep="\n"))
          next
        }
        # Slopes in concentration per year
        est <- list(p=est[2, 5],
                    slope=est[2, 3] * 365,
                    lower=est[2, 1] * 365, upper=est[2, 2] * 365,
                    intercept=est[1, 3],
                    intercept.lower=est[1, 2], intercept.upper=est[1, 1])

        tlim <- as.numeric(range(d.id.time$date)) # time in days

        est$slope.percent <- PercentChange(est$slope, est$intercept, tlim)
        est$lower.percent <- PercentChange(est$lower, est$intercept.lower, tlim)
        est$upper.percent <- PercentChange(est$upper, est$intercept.upper, tlim)

        rec <- cbind(rec, as.data.frame(est))

        # Mann Kendall

        ans <- MannKendall(d.id.time[, const])
        lst <- list(tau=ans$tau[1], sl=ans$sl[1], S=ans$S[1])
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Significant trend

        p <- rec[1, "p"]
        slope <- rec[1, "slope"]
        is.sig.trend <- p < 0.05
        is.pos.slope <- slope > 0
        if (is.sig.trend) {
          if (is.pos.slope)
            signif.trend <- "+"
          else
            signif.trend <- "-"
        } else {
          signif.trend <- "no trend"
        }
        lst <- list(signif.trend=signif.trend)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Add record to table
        tbl.out <- rbind(tbl.out, rec)
      }
    }
  }

tbl.out <- format(tbl.out)

write.table(tbl.out, file=file.out, quote=FALSE, sep="\t", row.names=FALSE)

invisible(tbl.out)
}
