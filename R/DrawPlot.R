DrawPlot <- function(obj, model, xlim=NULL, ylim=NULL, main=NULL, ylab=NULL) {

  obj$t1[is.na(obj$t1) & obj$is.left] <- 0
  if (!missing(model))
    obj <- obj[!is.na(obj$surv), ]

  xran <- extendrange(obj$Date, f=0.02)
  if (inherits(xlim, "Date")) {
    xlim[1] <- if (is.na(xlim[1])) xran[1] else xlim[1]
    xlim[2] <- if (is.na(xlim[2])) xran[2] else xlim[2]
  } else {
    xlim <- xran
  }
  if (is.null(ylim)) {
    ylim <- c(min(obj$t1, na.rm=TRUE), max(obj$t2, na.rm=TRUE))
    ylim <- extendrange(pretty(ylim), f=0.06)
  }

  par(mar=c(1, 3, 2, 0.5) + 0.1, mgp=c(2, 0.5, 0))
  plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", yaxs="i",
       xlab="Date", ylab=ylab, type="n", main=main, frame.plot=FALSE)

  if (missing(model)) {
    cols <- c("#F02311", "#107FC9", "#90AB76", "#BE80FF", "#050505")
    obj$col <- cols[as.integer(factor(obj$Name, levels=unique(obj$Name)))]
  } else {
    x <- seq(xlim[1], xlim[2], "days")
    y <- predict(model, list(Date=x), type="quantile", p=c(0.1, 0.9, 0.5))
    polygon(c(x, rev(x)), c(y[, 1], rev(y[, 2])), col="#FFFFD5", border=NA)
    lines(x, y[, 3], lty=1, lwd=1, col="#F02311")
    obj$col <- "#107FC9"
  }

  lwd <- 0.5 * (96 / (6 * 12))
  tcl <- 0.50 / (6 * par("csi"))

  is.int <- obj$is.left | obj$is.interval
  if (any(is.int)) {
    o <- obj[is.int, ]
    suppressWarnings(arrows(x0=o$Date, y0=o$t1, y1=o$t2, length=0.015, angle=90,
                            code=3, col=o$col, lwd=1))
  }
  if (any(obj$is.exact)) {
    o <- obj[obj$is.exact, ]
    points(x=o$Date, y=o$t2, pch=20, col=o$col)
  }

  at <- pretty(xlim, n=10)
  axis.Date(1, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis.Date(3, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)
  axis(2, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis(4, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)

  box(lwd=lwd)

  inset <- c(0.02, 0.02 * do.call("/", as.list(par("pin"))))
  if (missing(model)) {
    o <- obj[!duplicated(obj$Name), ]
    suppressWarnings(legend("topleft", o$Name, fill=o$col, border=NA, xpd=NA,
                            bg="#FFFFFFBB", box.lwd=lwd, inset=inset))
  } else {
    p <- 1 - pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)
    if (is.na(p))
      return()
    p <- ifelse(p < 0.001, "p < 0.001", paste("p =", sprintf("%.3f", p)))
    txt <- paste0("Regression, ", p)
    suppressWarnings(legend("topleft", txt, lty=1, col="#F02311", xpd=NA,
                            bg="#FFFFFFBB", box.lwd=lwd, inset=inset))
  }
}
