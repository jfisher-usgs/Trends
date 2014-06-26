



.MergePDFs <- function(path, retain.files=FALSE) {
  if (Sys.which("pdftk") == "")
    stop("pdftk not found, check that PDFtk Server is installed")

  pdfs <- list.files(path, pattern=".pdf$")
  if (length(pdfs) == 0 || pdfs == "")
    stop("path does not exist or input files are missing")

  out.pdf <- file.path(dirname(path), paste0(basename(path), ".pdf"))
  if(file.exists(out.pdf))
    file.remove(out.pdf)

  tmp.txt <- tempfile(fileext=".txt")
  tmp.bat <- tempfile(fileext=".bat")
  tmp.pdf <- tempfile(fileext=".pdf")

  cmd <- paste("cd", shQuote(path))

  cat("", file=tmp.bat)
  Sys.chmod(tmp.bat, mode="755")

  npages <- NULL
  for (i in pdfs) {
    cmd[2] <- paste("pdftk", shQuote(i), "dump_data output", shQuote(tmp.txt))
    cat(cmd, file=tmp.bat, sep="\n")
    system(command=tmp.bat, show.output.on.console=FALSE)
    txt <- scan(tmp.txt, what=character(), quiet=TRUE)
    npages <- c(npages, as.integer(txt[which(txt == "NumberOfPages:") + 1L]))
  }
  pages <- cumsum(npages) - (npages - 1L)

  cmd[2] <- paste("pdftk *.pdf cat output", shQuote(tmp.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "dump_data output",
                  shQuote(tmp.txt))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  bookmarks <- sub("\\.pdf$", "", pdfs)
  FUN <- function(i) {
    bookmark <- c("BookmarkBegin", paste("BookmarkTitle:", bookmarks[i]),
                  "BookmarkLevel: 1", paste("BookmarkPageNumber:", pages[i]))
    return(paste(bookmark, collapse="\n"))
  }
  bookmarks <- vapply(seq_along(bookmarks), FUN, "")
  cat(bookmarks, file=tmp.txt, sep="\n", append=TRUE)
  cmd[2] <- paste("pdftk", shQuote(tmp.pdf), "update_info", shQuote(tmp.txt),
                  "output", shQuote(out.pdf))
  cat(cmd, file=tmp.bat, sep="\n")
  system(command=tmp.bat, show.output.on.console=FALSE)

  unlink(tmp.txt)
  unlink(tmp.bat)
  unlink(tmp.pdf)
  if (!retain.files)
    unlink(path, recursive=TRUE, force=TRUE)

  invisible(out.pdf)
}



.OpenDevice <- function(path, id, graphics.type, w=8.5, h=11) {
  if (missing(graphics.type) || !graphics.type %in% c("pdf", "eps")) {
    dev.new(width=w, height=h)
  } else {
    file <- file.path(path, paste(id, graphics.type, sep="."))
    if (file.access(file, mode=0) == 0) {
      warning(paste("file already exists and will be overwritten:", file))
      remove.file(file)
    }
    if (graphics.type == "pdf") {
      pdf(file=file, width=w, height=h, version="1.6", colormodel="cmyk")
    } else if (graphics.type == "eps") {
      postscript(file=file, width=w, height=h, horizontal=FALSE, paper="letter")
    }
  }
  par(mfrow=c(4, 1), omi=c(4, 3.5, 6, 4.5) * 0.166667)
}



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
                    is.exact=NA, is.left=NA, is.interval=NA)
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
    is.exact <- !is.left & (is.na(d$sd) | is.na(d$dl))
    d$t1[is.exact] <- d$conc[is.exact]
    d$t2[is.exact] <- d$conc[is.exact]
    d$t2[is.left]  <- d$conc[is.left]
    lower.conc <- d$conc - 3 * d$sd
    upper.conc <- d$conc + 3 * d$sd

    are.left <- which(!is.exact & upper.conc <= d$dl)
    d$t2[are.left] <- d$dl[are.left]
    are.left <- which(!is.exact & lower.conc <= d$dl & upper.conc > d$dl)
    d$t2[are.left] <- upper.conc[are.left]

    are.interval <- which(!is.exact & lower.conc > d$dl)
    d$t1[are.interval] <- lower.conc[are.interval]
    d$t2[are.interval] <- upper.conc[are.interval]

    d$t2[which(d$t2 <= 0)] <- NA  # TODO: zero is invalid for 'lognormal' distribution
    d$t1[which(d$t1 <= 0 | is.na(d$t2))] <- NA

    is.t1 <- !is.na(d$t1)
    is.t2 <- !is.na(d$t2)
    d$is.exact    <-  is.t1 & is.t2 & d$t1 == d$t2
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
  d <- d[order(as.integer(factor(d$Site_id, levels=unique(d$Site_id)))), ]
  return(d)
}



.GetModelInfo <- function(model) {
  x <- c()
  x[c("c1", "c2")] <- model$coefficients
  x["scale"] <- model$scale
  x["p"] <- 1 - pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)
  x["slope"] <- 100 * (exp(model$coefficients[2]) - 1) * 365.242  # in percent change per year
  return(x)
}



.RunAnalysis <- function(processed.data, processed.config, sdate=NA, edate=NA,
                         graphics.type="pdf", merge.pdfs=TRUE, path=getwd(),
                         id=NULL, site.locations=NULL) {

  if (!is.na(sdate) && !inherits(sdate, "Date"))
    stop("incorrect class for argument 'sdate'")
  if (!is.na(edate) && !inherits(edate, "Date"))
    stop("incorrect class for argument 'edate'")

  obs <- list()
  models <- list()

  d <- processed.config[, c("Site_id", "Site_name", "Parameter")]
  stats <- data.frame(d, "sdate"=sdate, "edate"=edate, "n"=NA, "nmissing"=NA,
                      "nexact"=NA, "nleft"=NA, "ninterval"=NA, "nbelow.dl"=NA,
                      "min"=NA, "max"=NA, "median"=NA, "mean"=NA, "sd"=NA,
                      "0.95LCL"=NA, "0.95UCL"=NA, "c1"=NA, "c2"=NA, "scale"=NA,
                      "p"=NA, "slope"=NA, "trend"=NA, check.names=FALSE)

  for (i in seq_len(nrow(processed.config))) {
    d <- processed.data[[processed.config[i, "Parameter"]]]
    d <- d[d$Site_id == processed.config[i, "Site_id"], ]

    date1 <- if (is.na(sdate)) min(d$Date) else sdate
    date2 <- if (is.na(edate)) max(d$Date) else edate
    d <- d[d$Date >= date1 & d$Date <= date2, ]

    d$surv <- Surv(time=d$t1, time2=d$t2, type="interval2")
    is.missing <- is.na(d$surv)

    obs[[i]] <-  d

    stats[i, c("n", "nmissing")] <- c(nrow(d), sum(is.missing))
    vars <- c("nexact", "nleft", "ninterval")
    stats[i, vars] <- c(sum(d$is.exact), sum(d$is.left), sum(d$is.interval))
    stats[i, "nbelow.dl"] <- sum(d$code == "<" | (!is.na(d$dl) & d$t2 <= d$dl))

    stats[i, c("min", "max")] <- c(min(d$t1), max(d$t2))

    model <- suppressWarnings(survreg(surv ~ Date, data=d, dist="lognormal",
                                      control=survreg.control(maxiter=100)))
    models[[i]] <- model
    model.info <- .GetModelInfo(model)

    stats[i, names(model.info)] <- model.info
    p <- model.info["p"]
    slope <- model.info["slope"]
    is.trend <- !anyNA(c(p, slope)) & p <= 0.05
    stats[i, "trend"] <- ifelse(is.trend, ifelse(slope > 0, "+", "-"), "none")

    if (any(d$is.interval)) {
      fit <- summary(survfit(d$surv ~ 1))$table
      vars <- c("median", "0.95LCL", "0.95UCL")
      stats[i, vars] <- fit[vars]
    } else if (any(d$is.left)) {
      fit <- cenfit(d$t2[!is.missing], d$is.left[!is.missing])
      vars <- c("mean", "0.95LCL", "0.95UCL")
      stats[i, vars] <- suppressWarnings(mean(fit)[vars])
      stats[i, c("median", "sd")] <- suppressWarnings(c(median(fit), sd(fit)))
    } else {
      t1 <- na.omit(d$t1)
      stats[i, c("median", "mean", "sd")] <- c(median(t1), mean(t1), sd(t1))
    }
  }

  dir.create(path=file.path(path, id), showWarnings=FALSE, recursive=TRUE)

  plot.count <- list()
  for (i in seq_len(nrow(processed.config))) {
    site.id <- processed.config$Site_id[i]
    site.name <- processed.config$Site_name[i]
    if (is.null(plot.count[[site.id]]))
      plot.count[[site.id]] <- 0L
    if ((plot.count[[site.id]] + 4L) %% 4L == 0L) {
      if (graphics.type %in% c("pdf", "eps"))
        graphics.off()
      letter <- LETTERS[(plot.count[[site.id]] + 4L) %/% 4L]
      .OpenDevice(file.path(path, id), paste0(site.name, "_", letter),
                  graphics.type)
      main <- paste0(site.name, " (", site.id, ")")
    } else {
      main <- NULL
    }
    a <- attributes(processed.data[[processed.config$Parameter[i]]])
    ylab <- ifelse(is.na(a$Units), a$Name, paste0(a$Name, ", in ", a$Units))
    xlim <- if (inherits(c(sdate, edate), "Date")) c(sdate, edate) else NULL
    box.bg <- ifelse(graphics.type == "eps", "#FFFFFF", "#FFFFFFBB")
    .DrawSurvRegPlot(obs[[i]], models[[i]], xlim=xlim, main=main, ylab=ylab,
                     box.bg=box.bg)
    plot.count[[site.id]] <- plot.count[[site.id]] + 1L
  }
  if (graphics.type %in% c("pdf", "eps"))
    graphics.off()

  if (graphics.type == "pdf" && merge.pdfs)
    .MergePDFs(file.path(path, id))

  file <- file.path(path, paste0(id, ".tsv"))
  write.table(stats, file=file, quote=FALSE, sep="\t", row.names=FALSE)
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



.DrawSurvRegPlot <- function(obj, model, xlim=NULL, ylim=NULL, main=NULL,
                             ylab=NULL, box.bg="#FFFFFF") {
  obj <- obj[!is.na(obj$surv), ]
  obj$t1[is.na(obj$t1) & obj$is.left] <- 0

  xran <- extendrange(obj$Date, f=0.02)
  if (inherits(xlim, "Date")) {
    xlim[1] <- if (is.na(xlim[1])) xran[1] else xlim[1]
    xlim[2] <- if (is.na(xlim[2])) xran[2] else xlim[2]
  } else {
    xlim <- xran
  }
  if (is.null(ylim)) {
    ylim <- c(min(obj$t1), max(obj$t2, na.rm=TRUE))
    ylim <- extendrange(pretty(ylim), f=0.06)
  }

  par(mar=c(1, 3, 2, 1) + 0.1, mgp=c(2, 0.5, 0))
  plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", yaxs="i",
       xlab="Date", ylab=ylab, type="n", main=main, frame.plot=FALSE)

  x <- seq(xlim[1], xlim[2], "days")
  y <- predict(model, list(Date=x), type="quantile", p=c(0.1, 0.9, 0.5))
  polygon(c(x, rev(x)), c(y[, 1], rev(y[, 2])), col="#FFFFD5", border=NA)
  lines(x, y[, 3], lty=1, lwd=1, col="#F02311")

  is.int <- obj$is.left | obj$is.interval
  if (any(is.int)) {
    x  <- obj$Date[is.int]
    y1 <- obj$t1[is.int]
    y2 <- obj$t2[is.int]
    suppressWarnings(arrows(x, y1, x, y2, 0.01, 90, 3, col="#107FC9"))
  }
  is.pnt <- obj$is.exact
  if (any(is.pnt)) {
    x <- obj$Date[is.pnt]
    y <- obj$t2[is.pnt]
    points(x, y, pch=21, col="#107FC9", bg="#107FC9", cex=0.7)
  }

  lwd <- 0.5 * (96 / (6 * 12))
  tcl <- 0.50 / (6 * par("csi"))

  at <- pretty(xlim, n=10)
  axis.Date(1, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis.Date(3, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)
  axis(2, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis(4, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)

  box(lwd=lwd)

  p <- .GetModelInfo(model)["p"]
  if (is.na(p))
    return()
  p <- ifelse(p < 0.001, "< 0.001", paste("=", sprintf("%.3f", p)))
  p <- paste("p-value", p)
  txt <- c("Regression model", p)
  legend("topright", txt, lty=c(1, NA), col=c("#F02311", NA), xpd=NA,
         bg=box.bg, box.lwd=lwd)
}



.tmp <- function() {  # TODO: move to vignette

  require(NADA); require(rgdal)


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

  site.locations <- readOGR(path.in, layer = "Site_Locations", verbose = FALSE)

  path.out <- "C:/Users/jfisher/Desktop/Trends"

  ##


  file <- file.path(path.in, "Config_Cen.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1989-01-01"), edate = as.Date("2012-12-31"),
                        path = path.out, id = "Stats_1989-2012_Cen",
                        site.locations = site.locations)


  file <- file.path(path.in, "Config_Uncen.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1989-01-01"), edate = as.Date("2012-12-31"),
                        path = path.out, id = "Stats_1989-2012_Uncen",
                        site.locations = site.locations)


  file <- file.path(path.in, "Config_Uncen_Field.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1989-01-01"), edate = as.Date("2012-12-31"),
                        path = path.out, id = "Stats_1989-2012_Uncen_Field",
                        site.locations = site.locations)


  file <- file.path(path.in, "Config_Uncen_VOC.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1987-01-01"), edate = as.Date("2012-12-31"),
                        path = path.out, id = "Stats_1987-2012_Uncen_VOC",
                        site.locations = site.locations)


  file <- file.path(path.in, "Config_Cen_VOC.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1987-01-01"), edate = as.Date("2012-12-31"),
                        path = path.out, id = "Stats_1987-2012_Cen_VOC",
                        site.locations = site.locations)


  file <- file.path(path.in, "Config_RADS.tsv")
  config <- do.call(read.table, c(list(file), read.args))
  stats <- .RunAnalysis(processed.data, .ProcessConfig(config, processed.data),
                        sdate = as.Date("1981-01-01"), edate = as.Date("2013-12-31"),
                        path = path.out, id = "Stats_1981-2012_RAD",
                        site.locations = site.locations)


}
