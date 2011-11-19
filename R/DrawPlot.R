DrawPlot <- function(d, tbl.par, cen.var=NULL,
                     xlim=c(NA, NA), ylim=c(NA, NA),
                     regr=NULL, regr.lower=NULL, regr.upper=NULL,
                     regr.type="Regression line",
                     main=NULL, ylab=NULL, leg.box.col="#FFFFFF",
                     tick.lines=TRUE, p.value=NULL) {

  # Data and time
  dt.name <- names(d)[1]
  if (!inherits(d[[dt.name]], "POSIXct"))
    stop("Date-time values must be of class POSIXct")

  # Determine code variable for censored data
  cen.code <- NULL
  if (!is.null(cen.var)) {
    if (inherits(cen.var, "character"))
      cen.var <- which(names(d) == cen.var)
    if (!inherits(cen.var, c("numeric", "integer")) || length(cen.var) != 1) {
      stop("Censor code variable index is not valid")
    } else {
      cen.code <- d[, cen.var]
      if (!inherits(cen.code, "logical"))
        stop("Censor code variable is not of class logical")
    }
  }

  # Parameters
  p.names <- names(d)[-c(1, cen.var)]

  # Set x-axis limits
  origin <- as.POSIXct("1920-01-01 00:00:00.0")
  xlim <- as.POSIXct(xlim,  "%m/%d/%Y", tz="MST", origin=origin)
  xlim.default <- extendrange(d[[dt.name]])
  if (is.na(xlim[1]))
    xlim[1] <- xlim.default[1]
  if (is.na(xlim[2]))
    xlim[2] <- xlim.default[2]

  # Regression
  is.regr    <- inherits(regr, "function")
  is.regr[2] <- inherits(regr.lower, "function")
  is.regr[3] <- inherits(regr.upper, "function")

  # Set y-axis limits
  y <- d[, p.names]
  if (is.regr[1])
    y <- c(y, regr(xlim))
  if (is.regr[2])
    y <- c(y, regr.lower(xlim))
  if (is.regr[3])
    y <- c(y, regr.upper(xlim))
  ylim.default <- extendrange(y)
  if (ylim.default[1] < 0)
    ylim.default[1] <- 0
  if (is.na(ylim[1]))
    ylim[1] <- ylim.default[1]
  if (is.na(ylim[2]))
    ylim[2] <- ylim.default[2]

  # Initialize plot
  plot.new()
  plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")

  # Line width and length of tick marks
  lwd <- 0.5 * (96 / (6 * 12))
  tcl.major <- 0.50 / (6 * par("csi"))
  tcl.minor <- 0.25 / (6 * par("csi"))

  # Draw uncertainty as a polygon
  if (inherits(regr.lower, "function") & inherits(regr.upper, "function")) {
    x <- c(xlim, rev(xlim))
    y <- c(regr.lower(xlim), regr.upper(rev(xlim)))
    polygon(x, y, border=NA, col="#FFFFDD")
  }

  # Draw horizontal and vertical lines at major tick marks
  if (tick.lines) {
    h <- seq(par("yaxp")[1], par("yaxp")[2], length.out=par("yaxp")[3] + 1)
    v <- pretty(xlim)
    abline(h=h, v=v, col="lightgray", lwd=lwd)
  }

  # Draw y-axis
  axis(2, tcl=tcl.major, lwd=-1, lwd.ticks=lwd)
  axis(4, tcl=tcl.major, lwd=-1, lwd.ticks=lwd, labels=FALSE)

  # Draw major x-axis
  at.major <- pretty(xlim)
  axis.POSIXct(1, at=at.major, tcl=tcl.major, lwd=-1, lwd.ticks=lwd)
  axis.POSIXct(3, at=at.major, tcl=tcl.major, lwd=-1, lwd.ticks=lwd,
               labels=FALSE)

  # Draw minor x-axis
  mult <- 12
  num.at.major <- length(at.major) - 1
  no.match <- TRUE
  while (no.match) {
    at.minor <- pretty(xlim, n=num.at.major * mult)
    if (all(at.major %in% at.minor)) {
      at.minor <- at.minor[!at.minor %in% at.major]
      no.match <- FALSE
    } else if (mult > 1000) {
      warning("Problem with minor-tick marks on x-axis")
      break
    } else {
      mult <- mult + 1
    }
  }
  if (!no.match) {
    axis.POSIXct(1, at=at.minor, tcl=tcl.minor, lwd=-1, lwd.ticks=lwd,
                 labels=FALSE)
    axis.POSIXct(3, at=at.minor, tcl=tcl.minor, lwd=-1, lwd.ticks=lwd,
                 labels=FALSE)
  }

  # Draw y-axis label
  if (!is.null(ylab))
    title(ylab=ylab, cex.lab=1, line=3)

  # Draw main title
  if (!is.null(main))
    mtext(main, side=3, line=1)

  # Draw regression lines
  if (is.regr[1])
    lines(xlim, regr(xlim), lty=1, lwd=lwd)
  if (is.regr[2])
    lines(xlim, regr.lower(xlim), lty=2, lwd=lwd)
  if (is.regr[3])
    lines(xlim, regr.upper(xlim), lty=2, lwd=lwd)

  # Draw data points for each parameter and build legend

  leg.name <- leg.lty <- leg.pch <- leg.col <- leg.bg <- NULL
  for (p in p.names) {
    pch  <- tbl.par[p, "pch"]
    col  <- tbl.par[p, "col"]
    bg   <- tbl.par[p, "bg"]
    name <- tbl.par[p, "Name"]

    x <- d[, dt.name]
    y <- d[, p]

    if (is.null(cen.code) || all(!cen.code)) {
      points(x, y, pch=pch, col=col, bg=bg, lwd=lwd)
    } else {
      idxs <- which(cen.code)
      for (i in idxs) {
        lines(rep(x[i], 2), c(ylim[1], y[i]), lty=3, lwd=1.5)
      }
      points(x[-idxs], y[-idxs], pch=pch, col=col, bg=bg, lwd=lwd)
    }

    leg.name <- c(leg.name, name)
    leg.pch  <- c(leg.pch, pch)
    leg.col  <- c(leg.col, col)
    leg.bg   <- c(leg.bg, bg)
  }

  # Draw box around plot
  box(lwd=lwd)

  # Alter legend content if single parameter with regression line
  if (length(leg.name) == 1) {
    if (is.regr[1]) {
      leg.pch <- leg.bg <- NULL
      leg.col <- "#000000"
      leg.name <- regr.type
      if (inherits(p.value, "numeric"))
        leg.name <- paste(leg.name, " (p = ", sprintf("%.3f", p.value), ")",
                          sep="")
      leg.lty <- 1
      if (is.regr[2] || is.regr[3]) {
        leg.name[2] <- "95 percent confidence interval"
        leg.lty[2] <- 2
      }
    } else {
      return()
    }
  }

  # Draw legend
  legend(x=grconvertX(0.01, 'npc'), y=grconvertY(0.95, 'npc'),
         leg.name, lty=leg.lty, pch=leg.pch, col=leg.col, pt.bg=leg.bg,
         xpd=NA, bg=leg.box.col, bty="o", box.lwd=lwd, pt.lwd=lwd)
}
