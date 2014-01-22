RunTrendStats <- function(d, site.names, is.censored=FALSE, initial.dir=getwd(),
                     file.par=NULL, file.stats=NULL, write.tbl.out=FALSE,
                     file.out=NULL, figs.dir=NULL, gr.type="pdf",
                     cenken.tol=1e-12, cenken.iter=1e+6, dt.breaks=NULL,
                     xout=FALSE, draw.ci=FALSE) {
# This function performs a statistical analysis on uncensored and censored data
# tbl <- RunTrendStats(d, c("ANP 6", "ARBOR TEST"))

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

  # Calculate percentage change in slope and uncertainties
  # Temporal units must be consistant with slope
  PercentChange <- function(slope, intercept, tlim) {
    t1 <- as.numeric(tlim[1])
    t2 <- as.numeric(tlim[2])
    c1 <- slope * t1 + intercept
    c2 <- slope * t2 + intercept
    (((c2 / c1) - 1) / (t2 - t1)) * 100
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

  # Open and close graphics device
  GrDev <- function(site, plot.count) {
    if (gr.type != "windows")
      graphics.off()
    site <- paste(site, "_", LETTERS[((4L + plot.count) - 1L) %/% 4L], sep="")
    OpenGraphicsDevice(figs.dir, site, gr.type)
  }


  # Main program:

  options(stringsAsFactors=FALSE)

  # Paths
  file.par <- GetPath("config_para", file.par, initial.dir)
  file.stats <- GetPath("config_stat", file.stats, initial.dir)
  if (write.tbl.out)
    file.out <- GetPath("output_stat", file.out, initial.dir)
  if (gr.type != "windows")
    figs.dir <- GetPath("output_figs", figs.dir, initial.dir)

  # Read parameter configuration table
  tbl.par <- read.table(file=file.par, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE, comment.char="", row.names=1)
  row.names(tbl.par) <- make.names(row.names(tbl.par))

  # Determine background color for legend box
  if (gr.type == "postscript")
    leg.box.col <- "#FFFFFF"
  else
    leg.box.col <- "#FFFFFFBB"

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

  # Initialize plot count list
  plot.count <- list()

  # Loop though records in statistic table

  for (i in seq(along=idxs)) {
    idx <- idxs[i]

    id    <- tbl[idx, "Site_id"]
    site  <- tbl[idx, "Site_name"]
    sdate <- tbl[idx, "Start_date"]
    edate <- tbl[idx, "End_date"]

    p.names <- trim(unique(unlist(strsplit(tbl[idx, "Parameters"], ","))))
    parameters <- make.names(p.names)

    # Initialize plot count
    if (is.null(plot.count[[site]]))
      plot.count[[site]] <- 0L

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
      lst <- list("Site_id"=as.factor(id),
                  "Site_name"=site, "Parameter"=p.names[j],
                  "Start_date"=sdate, "End_date"=edate)
      rec <- as.data.frame(lst, optional=TRUE)

      # Determine pertinent column names in data table
      col.names <- c("Datetime", parameter)
      col.code.name <- paste(parameter, "_code", sep="")
      is.code <- col.code.name %in% d.names
      if (is.code)
        col.names <- c(col.names, col.code.name)
      else
        col.code.name <- NULL

      # Reduce size of data table
      is.id <- d$Site_id == id
      is.dt <- d$Datetime >= sdate & d$Datetime <= edate
      d.id <- d[is.id & is.dt, col.names]

      # Order dates
      d.id <- d.id[order(d.id$Datetime), ]

      # Determine the number of samples that are below the recording limit
      # and censored
      if (is.code)
        n.below.rl <- as.integer(sum(d.id[[col.code.name]] %in% 1:2))
      else
        n.below.rl <- 0L

      # Text to append to error messages
      err.extra <- paste("Row index: ", idx, ", Site name: ", site,
                         ", Parameter: ", parameter, "\n", sep="")

      # y-axis label
      ylab <- tbl.par[parameter, "Name"]
      if (!is.na(tbl.par[parameter, "Units"]))
        ylab <- paste(ylab, tbl.par[parameter, "Units"], sep=", in ")

      # Start statistical analysis

      # Censored data:
      if (is.censored) {

        # Remove any row with NA values
        d.id <- na.omit(d.id)
        n <- nrow(d.id)

        # Identify censored data
        if (is.code)
          is.cen <- d.id[[col.code.name]] %in% 1
        else
          is.cen <- rep(FALSE, nrow(d.id))

        # Warn if no censored data found
        if (!any(is.cen)) {
          txt <- "No censored data found in censored statistical analysis:"
          warning(paste(txt, err.extra, sep="\n"))
        }

        # Basic summary statistics
        dat <- d.id[[parameter]]
        len.record <- diff(range(d.id$Datetime))
        ans <- try(suppressWarnings(cenfit(dat, is.cen)), silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("NADA cenfit error:", err.extra, sep="\n"))
          next
        }
        cen.n      <- as.integer(ans@survfit$n)
        cen.n.cen  <- cen.n - as.integer(sum(ans@survfit$n.event))
        cen.median <- as.numeric(median(ans))
        cen.mean   <- as.numeric(mean(ans)["mean"])
        cen.sd     <- as.numeric(sd(ans))
        lst <- list("n"=cen.n, "n_above_rl"=cen.n - n.below.rl,
                    "n_cen"=cen.n.cen, "mean"=cen.mean, "median"=cen.median,
                    "min"=min(dat), "max"=max(dat), "std_dev"=cen.sd,
                    "len_record"=len.record)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Kendall's tau correlation coefficient and Akritas-Theil-Sen
        # nonparametric regression line, see ?cenken
        x <- as.numeric(d.id$Datetime)
        ans <- try(NADA:::kendallATS(y=dat, ycen=is.cen,
                                     x=x, xcen=rep(FALSE, length(x)),
                                     tol=cenken.tol, iter=cenken.iter),
                   silent=TRUE)
        if (inherits(ans, "try-error")) {
          warning(paste("NADA kendallATS error:", err.extra, sep="\n"))
          next
        }

        # Calculate percentage change per year in slopes
        slope.percent <- PercentChange(ans$slope, ans$intercept,
                                       tlim=range(d.id$Datetime))

        lst <- list("p_value"=ans$p, "tau"=ans$tau,
                    "slope"=ans$slope * secs.in.year, "int"=ans$intercept,
                    "slope_percent"=slope.percent * secs.in.year)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Nonparametric line
        if (is.na(ans$slope) || is.na(ans$intercept))
          regr <- NULL
        else
          regr <- function(x) {ans$slope * as.numeric(x) + ans$intercept}

        # Convert censored data code to logical
        if (is.code)
          d.id[[col.code.name]] <- d.id[[col.code.name]] == 1L

        # Draw plot
        plot.count[[site]] <- plot.count[[site]] + 1L
        if (((4L + plot.count[[site]]) - 1L) %% 4L == 0L) {
          GrDev(site, plot.count[[site]])
          main <- paste(site, " (", id, ")", sep="")
        } else {
          main <- NULL
        }
        DrawPlot(d.id, tbl.par[parameter, ], cen.var=col.code.name,
                 xlim=c(sdate, edate), regr=regr,
                 regr.type="Akritas-Theil-Sen line",
                 main=main, ylab=ylab, leg.box.col=leg.box.col, p.value=ans$p)

        # Classify trend
        ans <- ClassifyTrend(rec[1, "p_value"], rec[1, "slope"])
        lst <- list(trend=ans)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

      # Uncensored data:
      } else {

        # Warn if censored data found
        if (is.code) {
          n.cen <- as.integer(sum(d.id[[col.code.name]] %in% 1))
          if (n.cen > 0) {
            txt <- "Censored data found in uncensored statistical analysis:"
            warning(paste(txt, err.extra, sep="\n"))
          }
        } else {
          n.cen <- 0L
        }

        # Basic summary statistics
        dat <- na.omit(d.id[[parameter]])
        n <- length(dat)
        lst <- list("n"=n, "n_above_rl"=n - n.below.rl, "n_cen"=n.cen,
                    "mean"=mean(dat), "median"=median(dat),
                    "min"=min(dat), "max"=max(dat), "std_dev"=sd(dat))
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))

        # Length of temporal record
        tlim <- range(d.id$Datetime)
        len.record <- diff(tlim)
        lst <- list("len_record"=len.record)
        rec <- cbind(rec, as.data.frame(lst))

        # Average values over date-time intervals
        if (!is.null(dt.breaks)) {

          # Determine date cuts based on time interval
          ans <- try(cut(d.id$Datetime, dt.breaks), silent=TRUE)
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
            d.id <- na.omit(ans[, c("date", parameter)])
            names(d.id) <- c("Datetime", parameter)
          }
        }

        # Calculate Theil-Sen estimator and trend line using R.R. Wilcox'
        # functions
        x <- as.numeric(d.id$Datetime)
        y <- d.id[, parameter]

        est <- try(suppressWarnings(RunTheilSen(x=x, y=y, xout=xout)$regci),
                   silent=TRUE)
        if (inherits(est, "try-error") | is.null(est)) {
          warning(paste("Wilcox regci error:", err.extra, sep="\n"))
          next
        }
        est <- list("p_value"=est[2, 5],
                    "slope"=est[2, 3],
                    "lower"=est[2, 1],
                    "upper"=est[2, 2],
                    "int"=est[1, 3],
                    "int_lower"=est[1, 1],
                    "int_upper"=est[1, 2])

        # Regression line and confidence intervals
        regr <- regr.lower <- regr.upper <- NULL
        if (is.numeric(est$slope) && is.numeric(est$int)) {
          regr <- function(x) {est$slope * as.numeric(x) + est$int}
          if (draw.ci) {
            regr.lower <- function(x) {est$lower * as.numeric(x) + est$int_lower}
            regr.upper <- function(x) {est$upper * as.numeric(x) + est$int_upper}
          }
        }

        # Draw plot
        plot.count[[site]] <- plot.count[[site]] + 1L
        if (((4L + plot.count[[site]]) - 1L) %% 4L == 0L) {
          GrDev(site, plot.count[[site]])
          main <- paste(site, " (", id, ")", sep="")
        } else {
          main <- NULL
        }
        DrawPlot(d.id[, c("Datetime", parameter)],
                 tbl.par[parameter, ], xlim=c(sdate, edate),
                 regr=regr, regr.lower=regr.lower, regr.upper=regr.upper,
                 regr.type="Theil-Sen line", main=main, ylab=ylab,
                 leg.box.col=leg.box.col, p.value=est$p)

        # Calculate percentage change per year in slopes
        est$slope_percent <- PercentChange(est$slope, est$int, tlim)
        est$lower_percent <- PercentChange(est$lower, est$int_lower, tlim)
        est$upper_percent <- PercentChange(est$upper, est$int_upper, tlim)

        # Convert slopes from units per second to units per year
        est$slope <- est$slope * secs.in.year
        est$lower <- est$lower * secs.in.year
        est$upper <- est$upper * secs.in.year
        est$slope_percent <- est$slope_percent * secs.in.year
        est$lower_percent <- est$lower_percent * secs.in.year
        est$upper_percent <- est$upper_percent * secs.in.year
        rec <- cbind(rec, as.data.frame(est))

        # Classify trend
        trend <- ClassifyTrend(rec[1, "p_value"], rec[1, "slope"])
        lst <- list("trend"=trend)
        rec <- cbind(rec, as.data.frame(lst, optional=TRUE))
      }

      # Add statistics record to table
      tbl.out <- rbind(tbl.out, rec)
    }
  }

if (gr.type != "windows")
  graphics.off()

if (write.tbl.out)
  write.table(format(tbl.out, scientific=FALSE), file=file.out, quote=FALSE,
              sep="\t", row.names=FALSE)

invisible(tbl.out)
}
