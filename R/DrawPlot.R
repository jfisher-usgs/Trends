DrawPlot <- function(d, model, xlim=NULL, ylim=NULL, main=NULL, ylab="") {

  if (!inherits(d, "data.frame"))
    stop("wrong class for argument 'd'")
  if (nrow(d) == 0 || ncol(d) < 2)
    stop("incorrect dimension for argument 'd'")
  if (!inherits(d[, 1], "Date") || !inherits(d[, 2], "Surv"))
    stop("wrong class for fields in data frame 'd'")
  if (attr(d[, 2], "type") != "interval")
    stop("survival object must be of 'interval' type")
  if (!missing(model) && !inherits(model, "survreg"))
    stop("wrong class for agrument 'model'")

  d <- cbind(d[, 1:2], as.matrix(d[, 2]))
  if (any(d$status == 0))  # right censored
    stop("no right censored data allowed")
  d <- d[!is.na(d$status), , drop=FALSE]
  is.exact <- d$status == 1
  d$time2[is.exact] <- d$time1[is.exact]  # exact
  d$time2[d$status == 2] <- 0  # left censored

  xran <- extendrange(d[, 1], f=0.02)
  if (inherits(xlim, "Date")) {
    xlim[1] <- if (is.na(xlim[1])) xran[1] else xlim[1]
    xlim[2] <- if (is.na(xlim[2])) xran[2] else xlim[2]
  } else {
    xlim <- xran
  }
  if (is.null(ylim)) {
    ylim <- range(d[, c("time1", "time2")])
    ylim <- extendrange(pretty(ylim), f=0.06)
  }

  par(mar=c(1.5, 3, 1.5, 0.5) + 0.1, mgp=c(2, 0.5, 0))
  plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n", xaxs="i", yaxs="i",
       xlab="", ylab=ylab, type="n", main=main, frame.plot=FALSE)

  if (!missing(model)) {
    x <- seq(xlim[1], xlim[2], "days")
    newdata <- list(x)
    names(newdata) <- names(model$coefficients)[2]
    y <- predict(model, newdata, type="quantile", p=c(0.1, 0.9, 0.5))
    polygon(c(x, rev(x)), c(y[, 1], rev(y[, 2])), col="#FFFFD5", border=NA)
    lines(x, y[, 3], lty=1, lwd=1, col="#F02311")
  }

  lwd <- 0.5 * (96 / (6 * 12))
  tcl <- 0.50 / (6 * par("csi"))

  is.exact <- d$status == 1
  if (any(is.exact))
    points(x=d[is.exact, 1], y=d[is.exact, "time1"], pch=20, col="#107FC9")
  if (any(!is.exact))
    suppressWarnings(arrows(x0=d[!is.exact, 1], y0=d[!is.exact, "time1"],
                            y1=d[!is.exact, "time2"], length=0.015, angle=90,
                            code=3, col="#107FC9", lwd=1))

  at <- pretty(xlim, n=10)
  axis.Date(1, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis.Date(3, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)
  axis(2, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  axis(4, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)

  box(lwd=lwd)

  if (!missing(model)) {
    p <- 1 - pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)
    if (is.na(p))
      return()
    p <- ifelse(p < 0.001, "p < 0.001", paste("p =", sprintf("%.3f", p)))
    txt <- paste0("Regression, ", p)
    inset <- c(0.02, 0.02 * do.call("/", as.list(par("pin"))))
    suppressWarnings(legend("topleft", txt, lty=1, col="#F02311", xpd=NA,
                            bg="#FFFFFFBB", box.lwd=lwd, inset=inset))
  }
}
