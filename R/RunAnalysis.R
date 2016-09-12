#' Run Trend Analysis
#'
#' This function analyses observations for a significant trend.
#'
#' @param processed.obs list.
#'   See documentation for \code{\link{ProcessObs}} function for details.
#' @param processed.config data.frame.
#'   See documentation for \code{\link{ProcessConfig}} function for details.
#' @param path character.
#'   Path name of the folder where output data is written.
#' @param id character.
#'   An analysis identifier that is used to construct output file names.
#' @param sdate,edate Date or character.
#'   Start and end date corresponding to the period of interest, respectively.
#'   The required date format is YYYY-M-D (\code{\%Y-\%m-\%d}).
#' @param control list.
#'   Regression control values in the format produced by the \code{\link{survreg.control}} function.
#' @param sig.level numeric.
#'   Significance level to be coupled with the \emph{p}-value, see \sQuote{Value} section.
#' @param graphics.type character.
#'   Graphics type for plot figures.
#'   The default is the \sQuote{active} device, typically the normal screen device.
#'   A file-based device can be selected by specifying either \code{\link{pdf}} or \code{\link{postscript}}.
#' @param merge.pdfs logical.
#'   If true and \code{graphics.type = "pdf"} the figures are combined into a single PDF file,
#'   see documentation for \code{\link{MergePDFs}} function for details.
#' @param site.locations SpatialPointsDataFrame.
#'   Geo-referenced site coordinates with a required data.frame component of \code{Site_id},
#'   a unique site identifier.
#' @param is.seasonality logical.
#'   If true, seasonal patterns are modeled by a trigonometric regression;
#'   as covariates in the trend model.
#' @param explanatory.var data.frame.
#'   An explanatory variable added to the covariates of the trend model,
#'   see value from the \code{\link{ProcessWL}} function for the data table format.
#'   Explanatory variable values are linearly interpolated at sample dates in \code{processed.obs}.
#' @param is.residual logical.
#'   If true, the explanatory variable is transformed using its residuals from linear regression.
#'   Should be used when the explanatory variable is monotonically increasing or
#'   decreasing during the entire trend period.
#'   Requires specification of the \code{explanatory.var} argument.
#' @param thin.obs.mo character.
#'   Full name of a calendar month; if specified,
#'   data is thinned to one observation per year collected during this month.
#'   Allows verification that the variable sampling frequencies don't substantially affect the trend results.
#'   Thinning the data also could remove serial correlation in the more frequently sampled years.
#'
#' @details The \code{\link{survreg}} function is used to fit a parametric survival regression model
#'   to the observed data, both censored and uncensored.
#'   The specific class of survival model is known as the accelerated failure time (AFT) model.
#'   A maximum-likelihood estimation (MLE) method is used to estimate parameters in the AFT model.
#'   The MLE is solved by maximizing the log-likelihood using the Newton-Raphson method,
#'   an iterative root-finding algorithm.
#'   The likelihood function is dependent on the distribution of the observed data.
#'   Data is assumed to follow a log-normal distribution because
#'   most of the variables have values spanning two or more orders of magnitude.
#'   If all observations are uncensored, the survival regression becomes
#'   identical to ordinary least squares regression.
#'
#' @return Returns a data.frame object with the following components:
#'   \describe{
#'     \item{Site_id}{unique site identifier}
#'     \item{Site_name}{local site name}
#'     \item{Parameter_id}{unique parameter identifier}
#'     \item{Parameter_name}{common parameter name}
#'     \item{sdate,edate}{start and end date corresponding to the period of interest, respectively.}
#'     \item{n}{number of observations in the analysis.}
#'     \item{nmissing}{number of missing values.}
#'     \item{nexact}{number of exact (uncensored) observations.}
#'     \item{nleft}{number of left-censored observations.}
#'     \item{ninterval}{number of interval-censored observations.}
#'     \item{nbelow.rl}{number of observations that are below the reporting level.}
#'     \item{min,max}{minimum and maximum, respectively.}
#'     \item{median}{median}
#'     \item{mean,sd}{mean and standard deviation, respectively.
#'       Set to NA if censored data is present.}
#'     \item{iter}{number of Newton-Raphson iterations required for convergence.
#'       If NA, the regression failed or ran out of iterations and did not converge.}
#'     \item{slope}{slope of the linear trend over time in percent change per year.}
#'     \item{std.err}{standard error for the linear trend over time in percent change per year.}
#'     \item{p}{\emph{p}-value for the linear trend over time.}
#'     \item{p.model}{\emph{p}-value for the parametric survival regression model.}
#'     \item{trend}{significant trends are indicated by
#'       a \emph{p}-value (\code{p}) less than or equal to the significance level.
#'       The sign of the \code{slope} indicates whether the significant trend is positive (+) or negative (-).
#'       emph{p}-values greater than the significance level are specified as having no significant trend (none).}
#'   }
#'   If arguments \code{path} and \code{id} are specified,
#'   the returned data table of summary statistics (described above) is written to an external text file.
#'   If in addition a file-based graphics type is selected, plots are drawn to external files.
#'
#' @author J.C. Fisher and L.C. Davis, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{DrawPlot}}
#'
#' @keywords models
#'
#' @import survival
#'
#' @export
#'
#' @examples
#' # Specify global arguments for reading table formatted data in a text file
#' read.args <- list(header = TRUE, sep = "\t", colClasses = "character",
#'                   na.strings = "", fill = TRUE, strip.white = TRUE,
#'                   comment.char = "", flush = TRUE, stringsAsFactors = FALSE)
#'
#' # Read input files
#' path.in <- system.file("extdata", "SIR2014", package = "Trends")
#' file <- file.path(path.in, "Observations.tsv")
#' observations <- do.call(read.table, c(list(file), read.args))
#' file <- file.path(path.in, "Parameters.tsv")
#' parameters <- do.call(read.table, c(list(file), read.args))
#' file <- file.path(path.in, "Detection_Limits.tsv")
#' detection.limits <- do.call(read.table, c(list(file), read.args))
#' file <- file.path(path.in, "Config_VOC.tsv")
#' config <- do.call(read.table, c(list(file), read.args))
#'
#' # Process observations
#' processed.obs <- ProcessObs(observations, parameters, detection.limits,
#'                             date.fmt = "\%m/\%d/\%Y")
#'
#' # Plot data for a single parameter at a specific site
#' d <- processed.obs[["P32102"]]
#' d <- d[d$Site_id == "433002113021701", c("Date", "surv")]
#' DrawPlot(d, main = "RWMC Production", ylab = "Carbon Tetrachloride")
#'
#' # Configure sites, parameters, and duration for analysis
#' processed.config <- tail(ProcessConfig(config, processed.obs))
#'
#' # Run analysis
#' stats <- RunAnalysis(processed.obs, processed.config,
#'                      sdate = "1987-01-01", edate = "2012-12-31")

RunAnalysis <- function(processed.obs, processed.config, path, id, sdate=NA,
                        edate=NA, control=survreg.control(iter.max=100),
                        sig.level=0.05, graphics.type="", merge.pdfs=TRUE,
                        site.locations=NULL, is.seasonality=FALSE,
                        explanatory.var=NULL, is.residual=FALSE,
                        thin.obs.mo=NULL) {

  if ((missing(path) | missing(id)) & graphics.type %in% c("pdf", "postscript"))
    stop("arguments 'path' and 'id' are required for selected graphics type")

  if(inherits(sdate <- try(as.Date(sdate)), "try-error"))
    sdate <- NA
  if(inherits(edate <- try(as.Date(edate)), "try-error"))
    edate <- NA

  obs <- list()
  models <- list()

  d <- processed.config[, c("Site_id", "Site_name", "Parameter_id")]
  stats <- data.frame(d, "Parameter_name"=NA, "sdate"=sdate, "edate"=edate,
                      "n"=NA, "nmissing"=NA, "nexact"=NA, "nleft"=NA,
                      "ninterval"=NA, "nbelow.rl"=NA, "min"=NA, "max"=NA,
                      "median"=NA, "mean"=NA, "sd"=NA, "iter"=NA, "slope"=NA,
                      "std.err"=NA, "p"=NA, "p.model"=NA, "trend"=NA,
                      check.names=FALSE)

  for (i in seq_len(nrow(processed.config))) {
    d <- processed.obs[[processed.config[i, "Parameter_id"]]]

    site.id <- processed.config[i, "Site_id"]
    d <- d[d$Site_id == site.id, ]
    d <- d[order(d$Date), ]

    stats[i, "Parameter_name"] <- attr(d, "Parameter_name")

    date1 <- if (is.na(sdate)) min(d$Date) else sdate
    date2 <- if (is.na(edate)) max(d$Date) else edate
    d <- d[d$Date >= date1 & d$Date <= date2, ]

    if (!is.null(thin.obs.mo) && thin.obs.mo %in% month.name) {
      d <- d[months(d$Date) %in% thin.obs.mo, , drop=FALSE]
      d <- d[!duplicated(as.integer(format(d$Date, "%Y"))), , drop=FALSE]
      if (nrow(d) == 0)
        stop("thinning results in empty data set")
    }

    obs[[i]] <- d

    d <- cbind(d, as.matrix(d$surv))

    stats[i, c("n", "nmissing")] <- c(nrow(d), sum(is.na(d$status)))

    is.exact    <- d$status == 1
    is.left     <- d$status == 2
    is.interval <- d$status == 3
    is.below.rl <- d$code == "<"

    stats[i, "nexact"]    <- sum(is.exact)
    stats[i, "nleft"]     <- sum(is.left)
    stats[i, "ninterval"] <- sum(is.interval)
    stats[i, "nbelow.rl"] <- sum(is.below.rl)

    stats[i, c("min", "max")] <- range(d$time1, na.rm=TRUE)
    if (any(is.left))
      stats[i, "min"] <- 0

    is.explanatory <- is.data.frame(explanatory.var)
    if (is.explanatory) {
      e <- explanatory.var[explanatory.var$Site_id == site.id, ]
      e <- e[order(e$Date), -1]
      if (nrow(e) < 3)
        stop(paste("insufficient explanatory data:", site.id))
      if (is.residual) {
        ee <- e[e$Date >= min(d$Date) & e$Date <= max(d$Date), ]
        if (nrow(ee) < 3)
          stop(paste("insufficient residual explanatory data:", site.id))
        args <- alist(Date=NA, x=e$Date, y=e$Var, xx=ee$Date, yy=ee$Var)
        body <- quote(approx(x, y, xout=Date)$y -
                      stats::predict(lm(yy ~ xx), newdata=data.frame(xx=Date)))
      } else {
        args <- alist(Date=NA, x=e$Date, y=e$Var)
        body <- quote(approx(x, y, xout=Date)$y)
      }
      PredEx <- .MakeFunction(args, body)
    }

    x <- "surv ~ Date"
    if (is.explanatory)
      x <- c(x, "PredEx(Date)")
    if (is.seasonality)
      x <- c(x, "I(sin(2 * pi * as.numeric(Date) / 365.242))",
                "I(cos(2 * pi * as.numeric(Date) / 365.242))")
    formula <- stats::as.formula(paste(x, collapse=" + "))
    model <- suppressWarnings(survreg(formula, data=d, dist="lognormal",
                                      control=control, score=TRUE))

    is.converge <- model$iter < control$iter.max
    stats[i, "iter"] <- ifelse(is.converge, model$iter, NA)
    if (is.converge) {
      models[[i]] <- model
    } else {
      models[[i]] <- NA
      next()
    }

    summary.tbl <- summary(model)$table
    val <- summary.tbl["Date", "Value"]
    se  <- summary.tbl["Date", "Std. Error"]
    p   <- summary.tbl["Date", "p"]

    slope   <- 100 * (exp(val) - 1) * 365.242  # percent change per year
    std.err <- 100 * (exp(se)  - 1) * 365.242

    p.model <- 1 - stats::pchisq(2 * diff(model$loglik),
                                 sum(model$df) - model$idf)

    vars <-  c("slope", "std.err", "p", "p.model")
    stats[i, vars] <- c(slope, std.err, p, p.model)

    if (!anyNA(c(p, slope))) {
      is.trend <- p <= sig.level
      stats[i, "trend"] <- ifelse(is.trend, ifelse(slope > 0, "+", "-"), "none")
    }

    if (any(is.interval) || any(is.left)) {
      stats[i, "median"] <- stats::median(stats::predict(model, type="response"))
    } else {
      stats[i, "median"] <- stats::median(d$time1, na.rm=TRUE)
      stats[i, "mean"] <- mean(d$time1, na.rm=TRUE)
      stats[i, "sd"] <- stats::sd(d$time1, na.rm=TRUE)
    }
  }

  if (!missing(path) & !missing(id)) {
    file <- file.path(path, paste0(id, ".tsv"))
    utils::write.table(stats, file=file, quote=FALSE, sep="\t", row.names=FALSE)
  }

  is <- requireNamespace("sp", quietly=TRUE) &
        requireNamespace("rgdal", quietly=TRUE)
  if (is && inherits(site.locations, "SpatialPointsDataFrame")) {
    idxs <- match(stats$Site_id, site.locations@data$Site_id)
    if (anyNA(idxs)) {
      stop("site id(s) not found in 'spatial.locations'")
    } else {
      coords <- site.locations@coords[idxs, , drop=FALSE]
      crs <- site.locations@proj4string
      obj <- sp::SpatialPointsDataFrame(coords, stats, proj4string=crs)
      suppressWarnings(rgdal::writeOGR(obj, path, id, "ESRI Shapefile",
                                       check_exists=TRUE, overwrite_layer=TRUE))
    }
  }


  id.path <- .CreateDir(path, id, graphics.type)

  figs <- NULL
  plot.count <- list()
  for (i in seq_len(nrow(processed.config))) {
    site.id <- processed.config$Site_id[i]
    site.name <- processed.config$Site_name[i]

    if (is.null(plot.count[[site.id]]))
      plot.count[[site.id]] <- 0L
    if ((plot.count[[site.id]] + 4L) %% 4L == 0L) {
      letter <- LETTERS[(plot.count[[site.id]] + 4L) %/% 4L]
      fig <- paste0(site.name, "_", letter)
      .OpenDevice(id.path,  fig, graphics.type)
      figs <- c(figs, fig)
      main <- paste0(site.name, " (", site.id, ")")
    } else {
      main <- NULL
    }
    plot.count[[site.id]] <- plot.count[[site.id]] + 1L

    a <- attributes(processed.obs[[processed.config$Parameter_id[i]]])
    ylab <- ifelse(is.na(a$Units), a$Parameter_name,
                   paste0(a$Parameter_name, ", in ", a$Units))
    xlim <- if (inherits(c(sdate, edate), "Date")) c(sdate, edate) else NULL
    DrawPlot(obs[[i]][, c("Date", "surv")], models[[i]],
             xlim=xlim, main=main, ylab=ylab)
  }
  if (graphics.type %in% c("pdf", "postscript"))
    grDevices::graphics.off()
  if (graphics.type == "pdf" && merge.pdfs) {
    if (as.logical(nchar(Sys.which("pdftk"))))
      MergePDFs(id.path, paste0(figs, ".pdf"))
    else
      warning("PDFtk Server cannot be found so PDF files will not be merged")
  }

  invisible(stats)
}


.CreateDir <- function(path, id, graphics.type) {
  if (!graphics.type %in% c("pdf", "postscript"))
    return()
  dir.create(path=path, showWarnings=FALSE, recursive=TRUE)
  ext <- ifelse(graphics.type == "postscript", "eps", "pdf")
  id.path <- file.path(path, id)
  if (file.exists(id.path))
    unlink(file.path(id.path, paste0("*.", ext)))
  dir.create(path=id.path, showWarnings=FALSE, recursive=TRUE)
  return(id.path)
}


.OpenDevice <- function(path, id, graphics.type, w=8.5, h=11) {
  if (missing(graphics.type) || !graphics.type %in% c("pdf", "postscript")) {
    grDevices::dev.new(width=w, height=h)
  } else {
    grDevices::graphics.off()
    ext <- ifelse(graphics.type == "postscript", "eps", "pdf")
    file <- file.path(path, paste(id, ext, sep="."))
    if (file.access(file) == 0) {
      warning(paste("file already exists and will be overwritten:", file))
    }
    if (graphics.type == "postscript") {
      grDevices::postscript(file=file, width=w, height=h, horizontal=FALSE,
                            paper="letter")
    } else {
      grDevices::pdf(file=file, width=w, height=h, version="1.6",
                     colormodel="cmyk")
    }
  }
  graphics::par(mfrow=c(4, 1), omi=c(4, 3.5, 6, 4.5) * 0.166667)
  invisible()
}


.MakeFunction <- function(args, body, env=parent.frame()) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}
