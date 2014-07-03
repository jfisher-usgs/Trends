RunAnalysis <- function(processed.obs, processed.config, path, id, sdate=NA,
                        edate=NA, graphics.type="", merge.pdfs=TRUE,
                        site.locations=NULL) {

  if ((missing(path) | missing(id)) & graphics.type %in% c("pdf", "postscript"))
    stop("arguments 'path' and 'id' are required for selected graphics type")

  if(inherits(sdate <- try(as.Date(sdate)), "ty-error"))
    sdate <- NA
  if(inherits(edate <- try(as.Date(edate)), "ty-error"))
    edate <- NA

  obs <- list()
  models <- list()

  d <- processed.config[, c("Site_id", "Site_name", "Parameter")]
  stats <- data.frame(d, "sdate"=sdate, "edate"=edate, "n"=NA, "nmissing"=NA,
                      "nexact"=NA, "nleft"=NA, "ninterval"=NA, "nbelow.rl"=NA,
                      "min"=NA, "max"=NA, "median"=NA, "mean"=NA, "sd"=NA,
                      "0.95LCL"=NA, "0.95UCL"=NA, "c1"=NA, "c2"=NA, "scale"=NA,
                      "p"=NA, "slope"=NA, "trend"=NA, check.names=FALSE)

  for (i in seq_len(nrow(processed.config))) {
    d <- processed.obs[[processed.config[i, "Parameter"]]]
    d <- d[d$Site_id == processed.config[i, "Site_id"], ]

    date1 <- if (is.na(sdate)) min(d$Date) else sdate
    date2 <- if (is.na(edate)) max(d$Date) else edate
    d <- d[d$Date >= date1 & d$Date <= date2, ]
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

    stats[i, c("min", "max")] <-  range(d$time1, na.rm=TRUE)
    if (any(is.left))
      stats[i, "min"] <- 0

    model <- suppressWarnings(survreg(surv ~ Date, data=d, dist="lognormal",
                                      control=survreg.control(maxiter=100)))
    models[[i]] <- model

    p <- 1 - pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)
    slope <- 100 * (exp(model$coefficients[2]) - 1) * 365.242  # % change per yr

    model.info <- c()
    model.info[c("c1", "c2")] <- model$coefficients
    model.info["scale"] <- model$scale
    model.info["p"] <- p
    model.info["slope"] <- slope
    stats[i, names(model.info)] <- model.info

    is.trend <- !anyNA(c(p, slope)) & p <= 0.05
    stats[i, "trend"] <- ifelse(is.trend, ifelse(slope > 0, "+", "-"), "none")

    if (any(is.interval)) {
      fit <- summary(survfit(d$surv ~ 1))$table
      vars <- c("median", "0.95LCL", "0.95UCL")
      stats[i, vars] <- fit[vars]
    } else if (any(is.left)) {
      is.not.missing <- !is.na(d$status)
      fit <- cenfit(d$time1[is.not.missing], is.left[is.not.missing])
      vars <- c("mean", "0.95LCL", "0.95UCL")
      stats[i, vars] <- suppressWarnings(mean(fit)[vars])
      stats[i, "median"] <- suppressWarnings(median(fit))
      stats[i, "sd"]     <- suppressWarnings(sd(fit))
    } else {
      stats[i, "median"] <- median(d$time1, na.rm=TRUE)
      stats[i, "mean"]   <- mean(d$time1, na.rm=TRUE)
      stats[i, "sd"]     <- sd(d$time1, na.rm=TRUE)
    }
  }

  id.path <- .CreateDir(path, id, graphics.type)

  plot.count <- list()
  for (i in seq_len(nrow(processed.config))) {
    site.id <- processed.config$Site_id[i]
    site.name <- processed.config$Site_name[i]

    if (is.null(plot.count[[site.id]]))
      plot.count[[site.id]] <- 0L
    if ((plot.count[[site.id]] + 4L) %% 4L == 0L) {
      letter <- LETTERS[(plot.count[[site.id]] + 4L) %/% 4L]
      .OpenDevice(id.path, paste0(site.name, "_", letter), graphics.type)
      main <- paste0(site.name, " (", site.id, ")")
    } else {
      main <- NULL
    }
    plot.count[[site.id]] <- plot.count[[site.id]] + 1L

    a <- attributes(processed.obs[[processed.config$Parameter[i]]])
    ylab <- ifelse(is.na(a$Units), a$Name, paste0(a$Name, ", in ", a$Units))
    xlim <- if (inherits(c(sdate, edate), "Date")) c(sdate, edate) else NULL
    DrawPlot(obs[[i]][, c("Date", "surv")], models[[i]],
             xlim=xlim, main=main, ylab=ylab)
  }
  if (graphics.type %in% c("pdf", "postscript"))
    graphics.off()
  if (graphics.type == "pdf" && merge.pdfs)
    MergePDFs(id.path)

  if (!missing(path) & !missing(id)) {
    file <- file.path(path, paste0(id, ".tsv"))
    write.table(stats, file=file, quote=FALSE, sep="\t", row.names=FALSE)
  }
  if (inherits(site.locations, "SpatialPointsDataFrame")) {
    idxs <- match(stats$Site_id, site.locations@data$Site_id)
    if (anyNA(idxs)) {
      stop("site id(s) not found in 'spatial.locations'")
    } else {
      coords <- site.locations@coords[idxs, , drop=FALSE]
      crs <- site.locations@proj4string
      obj <- SpatialPointsDataFrame(coords, stats, proj4string=crs)
      writeOGR(obj, path, id, "ESRI Shapefile", check_exists=TRUE,
               overwrite_layer=TRUE)
    }
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
}
