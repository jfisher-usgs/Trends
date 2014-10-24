RunAnalysis <- function(processed.obs, processed.config, path, id, sdate=NA,
                        edate=NA, control=survreg.control(iter.max=100),
                        sig.level=0.05, graphics.type="", merge.pdfs=TRUE,
                        site.locations=NULL, is.seasonality=FALSE,
                        explanatory.var=NULL, is.residual=FALSE,
                        thin.obs.mo=NULL) {

  if ((missing(path) | missing(id)) & graphics.type %in% c("pdf", "postscript"))
    stop("arguments 'path' and 'id' are required for selected graphics type")

  if(inherits(sdate <- try(as.Date(sdate)), "ty-error"))
    sdate <- NA
  if(inherits(edate <- try(as.Date(edate)), "ty-error"))
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
    d <- d[d$Site_id == processed.config[i, "Site_id"], ]

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
      idxs <- explanatory.var$Site_id == site.id
      if (sum(idxs) < 3)
        stop(paste("insufficient explanatory data:", site.id))
      e <- explanatory.var[idxs, -1]
      e <- e[order(e[, 1]), ]
      d$explanatory.var <- approx(e[, 1], e[, 2], xout=d$Date)$y
      if (anyNA(d$explanatory.var))
        stop(paste("unable to predict explanatory variable:", site.id))
      if (is.residual) {
        d$explanatory.var <- residuals(lm(explanatory.var ~ Date, data=d))
      }
    }

    x <- "surv ~ Date"
    if (is.explanatory)
      x <- c(x, "explanatory.var")
    if (is.seasonality)
      x <- c(x, "I(sin(2 * pi * as.numeric(Date) / 365.242))",
                "I(cos(2 * pi * as.numeric(Date) / 365.242))")
    formula <- as.formula(paste(x, collapse=" + "))
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

    p.model <- 1 - pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)

    vars <-  c("slope", "std.err", "p", "p.model")
    stats[i, vars] <- c(slope, std.err, p, p.model)

    if (!anyNA(c(p, slope))) {
      is.trend <- p <= sig.level
      stats[i, "trend"] <- ifelse(is.trend, ifelse(slope > 0, "+", "-"), "none")
    }

    if (any(is.interval) || any(is.left)) {
      stats[i, "median"] <- median(predict(model, type="response"))
    } else {
      stats[i, "median"] <- median(d$time1, na.rm=TRUE)
      stats[i, "mean"] <- mean(d$time1, na.rm=TRUE)
      stats[i, "sd"] <- sd(d$time1, na.rm=TRUE)
    }
  }

  if (!missing(path) & !missing(id)) {
    file <- file.path(path, paste0(id, ".tsv"))
    write.table(stats, file=file, quote=FALSE, sep="\t", row.names=FALSE)
  }

  is.rgdal <- suppressPackageStartupMessages(require("rgdal", quietly=TRUE))
  if (is.rgdal && inherits(site.locations, "SpatialPointsDataFrame")) {
    idxs <- match(stats$Site_id, site.locations@data$Site_id)
    if (anyNA(idxs)) {
      stop("site id(s) not found in 'spatial.locations'")
    } else {
      coords <- site.locations@coords[idxs, , drop=FALSE]
      crs <- site.locations@proj4string
      obj <- SpatialPointsDataFrame(coords, stats, proj4string=crs)
      suppressWarnings(writeOGR(obj, path, id, "ESRI Shapefile",
                                check_exists=TRUE, overwrite_layer=TRUE))
    }
  }

  if (is.explanatory)
    return(stats)

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
    graphics.off()
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
    dev.new(width=w, height=h)
  } else {
    graphics.off()
    ext <- ifelse(graphics.type == "postscript", "eps", "pdf")
    file <- file.path(path, paste(id, ext, sep="."))
    if (file.access(file) == 0) {
      warning(paste("file already exists and will be overwritten:", file))
    }
    if (graphics.type == "postscript") {
      postscript(file=file, width=w, height=h, horizontal=FALSE, paper="letter")
    } else {
      pdf(file=file, width=w, height=h, version="1.6", colormodel="cmyk")
    }
  }
  par(mfrow=c(4, 1), omi=c(4, 3.5, 6, 4.5) * 0.166667)
  invisible()
}
