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

  # Calculate percentage change in slope and uncertainties.
  # Need start and end times (t) in seconds to work out concentrations at those
  # points. Slope is given in concentration per year. Returns percent change in
  # percent per year.
  PercentChange <- function(slope, intercept, tlim) {
    t1 <- tlim[1]
    t2 <- tlim[2]
    c1 <- slope * (t1 / secs.in.year) + intercept
    c2 <- slope * (t2 / secs.in.year) + intercept
    (((c2 / c1) - 1) / (t2 - t1)) * secs.in.year * 100
  }

  # Classify trend using p-value and slope
  ClassifyTrend <- function(p, slope, tol=0.05) {
    is.sig.trend <- p <= tol
    is.pos.slope <- slope > 0
    if (is.sig.trend) {
      if (is.pos.slope)
        trend <- "+"
      else
        trend <- "-"
    } else {
      trend <- "no trend"
    }
    trend
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

  # Number of seconds in year, used for time conversions
  secs.in.year <- 31536000

  # Loop though records in statistic table

  for (i in seq(along=idxs)) {
    idx <- idxs[i]

    id     <- tbl[idx, "Site_id"]
    site   <- tbl[idx, "Site_name"]
    sdate  <- tbl[idx, "Start_date"]
    edate  <- tbl[idx, "End_date"]

    p.names <- trim(unique(unlist(strsplit(tbl[idx, "Parameters"], ","))))
    parameters <- make.names(p.names)

    # Loop through parameters in record

    for (j in seq(along=parameters)) {
      parameter <- parameters[j]
      if (!parameter %in% d.names) {
        txt <- paste("Parameter is not recognized and will be skipped:\n",
                     "Row index: ", idx, ", Column name: Parameters, ",
                     "String: ", parameter, "\n", sep="")
        warning(txt)
        next
      }

      # Start record that will be added to output table
      lst <- list("Site_id"=format(id, scientific=FALSE),
                  "Site_name"=site, "Parameter"=p.names[j],
                  "Start_date"=sdate, "End_date"=edate)
      rec <- as.data.frame(lst, optional=TRUE)

      # Determine pertinent column names in data table
      col.names <- c("datetime", parameter)
      col.code.name <- paste(parameter, "_code", sep="")
      is.code <- col.code.name %in% d.names
      if (is.code)
        col.names <- c(col.names, col.code.name)

      # Reduce size of data table
      is.id <- d$Site_id == id
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
      err.extra <- paste("Row index: ", idx, ", Site name: ", site,
                         ", Parameter: ", parameter, "\n", sep="")

      # Start statistical analysis

      # Censored data
      if (is.censored) {

        # Remove any row with NA values
        d.id <- na.omit(d.id)
        n <- nrow(d.id)

        # Identify censored data
        if (is.code)
          is.cen <- d.id[[col.code.name]] %in% 1
        else
          is.cen <- rep(FALSE, nrow(d.id))
        n.cen <- sum(as.integer(is.cen))

        # Basic summary statistics
        dat <- d.id[[parameter]]
        ans <- try(suppressWarnings(cenfit(dat, is.cen)), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Cenfit error:", err.extra, sep="\n"))
          next
        }
        len.record <- diff(range(d.id$datetime))
        lst <- list("n"=n, "n_cen"=n.cen,
                    "mean"=as.numeric(mean(ans)[1]), "median"=median(ans),
                    "min"=min(dat), "max"=max(dat),
                    "std_dev"=sd(ans), "len_record"=len.record)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Kendall's tau correlation coefficient and trend line
        ans <- try(suppressWarnings(cenken(dat, is.cen,
                                           as.numeric(d.id$datetime))),
                                           silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Cenken error:", err.extra, sep="\n"))
          next
        }
        lst <- list("slope"=ans$slope * secs.in.year, "intercept"=ans$intercept,
                    "tau"=ans$tau, "p"=ans$p)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Classify trend
        ans <- ClassifyTrend(rec[1, "p"], rec[1, "slope"])
        lst <- list(trend=ans)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))









        # Draw plot

#       cenxyplot(d.id$datetime, FALSE, dat, is.cen, main=err.extra)

#       if (!is.na(rec[1, "slope"]) && !is.na(rec[1, "intercept"]))
#         lines(ans, col="red")

##      if (ans$p < 0.05)
##        browser()









      # Uncensored data
      } else {

        # Warn if censored data found
        is.cen <- any(d.id[[col.code.name]] %in% 1)
        if (is.cen) {
          txt <- "Censored data found in uncensored statistical analysis:"
          warning(paste(txt, err.extra, sep="\n"))
          next
        }

        # Basic summary statistics
        dat <- na.omit(d.id[[parameter]])
        n <- length(dat)
        lst <- list("n"=n, "n_above_rl"=n - below.rl,
                    "mean"=mean(dat), "median"=median(dat),
                    "min"=min(dat), "max"=max(dat), "std_dev"=sd(dat))
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Find date cuts based on time interval
        ans <- try(cut(d.id$datetime, avg.time), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Time cut error:", ans, err.extra, sep="\n"))
          next
        } else {
          d.id.cuts <- as.POSIXct(ans, "%Y-%m-%d", tz="MST", origin=origin)
        }

        # Time average parameter based on date cuts
        ans <- try(aggregate(d.id, list(date=d.id.cuts),
                             function(i) mean(i, na.rm=TRUE)), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("Time average error:", ans, err.extra, sep="\n"))
          next
        } else {
          d.id.time <- na.omit(ans[, c("date", parameter)])
        }
        len.record <- diff(range(d.id.time$date))
        lst <- list("len_record"=len.record)
        rec <- cbind(rec, as.data.frame(lst))

        # Calculate Theil-Sen estimator and trend line using R.R. Wilcox'
        # functions

        est <- try(suppressWarnings(regci(as.numeric(d.id.time$date),
                                          d.id.time[, parameter], alpha=0.5,
                                          pr=FALSE)$regci), silent=TRUE)
        if (inherits(est, "try-error") | is.null(est)) {
          warning(paste("regci error:", err.extra, sep="\n"))
          next
        }

        # Convert slopes from unit per second to unit per year;
        est <- list("p"=est[2, 5],
                    "slope"=est[2, 3] * secs.in.year,
                    "lower"=est[2, 1] * secs.in.year,
                    "upper"=est[2, 2] * secs.in.year,
                    "intercept"=est[1, 3],
                    "intercept_lower"=est[1, 1],
                    "intercept_upper"=est[1, 2])

        # Time limit in seconds
        tlim <- as.numeric(range(d.id.time$date))

        est$slope_percent <- PercentChange(est$slope, est$intercept, tlim)
        est$lower_percent <- PercentChange(est$lower, est$intercept_lower, tlim)
        est$upper_percent <- PercentChange(est$upper, est$intercept_upper, tlim)

        rec <- cbind(rec, as.data.frame(est))

        # Calculate Kendall estimates using the Kendall package
        ans <- MannKendall(d.id.time[, parameter])
        lst <- list("Kendall_tau"=ans$tau[1],
                    "Kendall_sl"=ans$sl[1],
                    "Kendall_S"=ans$S[1])
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Classify trend
        trend <- ClassifyTrend(rec[1, "p"], rec[1, "slope"])
        lst <- list("trend"=trend)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))
      }

      # Add record to table
      tbl.out <- rbind(tbl.out, rec)
    }
  }


tbl.out <- format(tbl.out)

write.table(tbl.out, file=file.out, quote=FALSE, sep="\t", row.names=FALSE)

invisible(tbl.out)
}
