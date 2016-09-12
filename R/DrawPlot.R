#' Draw Single Plot
#'
#' This function draws a single time-series plot on the active graphics device.
#'
#' @param d data.frame.
#'   Observations, where the first and second column is of class Date and \link{Surv}, respectively.
#'   The Surv component is of type \emph{interval} and should not contain right-censored data.
#' @param model \link{survreg}.
#'   Survival regression model
#' @param plim numeric.
#'   Percentile limits defining the confidence band of the regression model.
#' @param xlim Date.
#'   The \emph{x} limits of plot.
#' @param ylim numeric.
#'   The \emph{y} limits of plot.
#' @param main character.
#'   Main plot title
#' @param ylab character.
#'   Label for \emph{y} axis.
#'
#' @details Uncensored data is drawn as points.
#'   Left- and interval-censored data is drawn as vertical lines with short-horizontal lines at the interval bounds.
#'   Note that the lower bound of left-censored data is placed at zero.
#'   The 50th percentile of the regression model is drawn as a curved line.
#'   The confidence band is drawn as a solid polygon.
#'
#' @return Used for the side-effect of a new plot generated.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{RunAnalysis}}, \code{\link[survival]{predict.survreg}}
#'
#' @keywords hplot
#'
#' @import survival
#'
#' @export
#'
#' @examples
#' # Create time-series data set with uncensored, left-censored, and interval-censored data
#' n <- 30
#' x <- as.Date(sort(sample(1:1000, n)), origin = as.Date("1992-02-15"))
#' time1 <- runif(n, min = 0, max = 100)
#' time2 <- time1 + runif(n, min = 0, max = 10)
#' time1[sample(1:n, 10)] <- NA
#' idxs <- sample(1:n, 10)
#' time1[idxs] <- time2[idxs]
#' y <- survival::Surv(time1, time2, type = "interval2")
#' d <- data.frame(x, y)
#'
#' # Fit the data with a regression model
#' model <- survival::survreg(y ~ x, data = d)
#'
#' # Plot the data and regression model
#' DrawPlot(d, model, plim = c(NA, 0.8), main = "Title", ylab = "Value")
#'

DrawPlot <- function(d, model, plim=c(0.1, 0.9), xlim=NULL, ylim=NULL,
                     main=NULL, ylab="") {

  if (!inherits(d, "data.frame"))
    stop("wrong class for argument 'd'")
  if (nrow(d) == 0 || ncol(d) < 2)
    stop("incorrect dimension for argument 'd'")
  if (!inherits(d[, 1], "Date") || !inherits(d[, 2], "Surv"))
    stop("wrong class for fields in data frame 'd'")
  if (attr(d[, 2], "type") != "interval")
    stop("survival object must be of 'interval' type")
  is.model <- ifelse(missing(model) || is.na(model), FALSE, TRUE)
  if (is.model && !inherits(model, "survreg"))
    stop("wrong class for agrument 'model'")

  if (!is.null(plim)) {
    if (!is.numeric(plim) || length(plim) != 2 ||
        identical(plim[1], plim[2])) {
      warning("problem with argument 'plim', confidence band not drawn")
      plim <- NULL
    } else {
      plim[is.na(plim)] <- 0.5
    }
  }

  d <- cbind(d[, 1:2], as.matrix(d[, 2]))
  if (any(d$status == 0))  # right censored
    stop("right-censored data is not allowed")
  d <- d[!is.na(d$status), , drop=FALSE]
  is.exact <- d$status == 1
  d$time2[is.exact] <- d$time1[is.exact]  # uncensored
  d$time2[d$status == 2] <- 0  # left censored

  xran <- grDevices::extendrange(d[, 1], f=0.02)
  if (inherits(xlim, "Date")) {
    xlim[1] <- if (is.na(xlim[1])) xran[1] else xlim[1]
    xlim[2] <- if (is.na(xlim[2])) xran[2] else xlim[2]
  } else {
    xlim <- xran
  }
  if (is.null(ylim)) {
    ylim <- range(d[, c("time1", "time2")])
    ylim <- grDevices::extendrange(pretty(ylim), f=0.06)
  }

  graphics::par(mar=c(1.5, 3, 1.5, 1) + 0.1, mgp=c(2, 0.5, 0))
  graphics::plot(NA, xlim=xlim, ylim=ylim, xaxt="n", yaxt="n",
                 xaxs="i", yaxs="i", xlab="", ylab=ylab, type="n",
                 main=main, frame.plot=FALSE)

  if (is.model) {
    x <- seq(xlim[1], xlim[2], "days")
    newdata <- list(x)
    names(newdata) <- names(model$coefficients)[2]
    if (!is.null(plim)) {
      y <- stats::predict(model, newdata, type="quantile", p=plim)
      graphics::polygon(c(x, rev(x)), c(y[, 1], rev(y[, 2])), col="#FFFFD5",
                        border=NA)
    }
    y <- stats::predict(model, newdata, type="quantile", p=0.5)
    graphics::lines(x, y, lty=1, lwd=1, col="#F02311")
  }

  lwd <- 0.5 * (96 / (6 * 12))
  tcl <- 0.50 / (6 * graphics::par("csi"))

  is.exact <- d$status == 1
  if (any(is.exact))
    graphics::points(x=d[is.exact, 1], y=d[is.exact, "time1"], pch=20,
                     col="#107FC9")
  if (any(!is.exact))
    suppressWarnings(graphics::arrows(x0=d[!is.exact, 1],
                                      y0=d[!is.exact, "time1"],
                                      y1=d[!is.exact, "time2"], length=0.015,
                                      angle=90, code=3, col="#107FC9", lwd=1))

  at <- pretty(xlim, n=10)
  graphics::axis.Date(1, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  graphics::axis.Date(3, xlim, at, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)
  graphics::axis(2, tcl=tcl, lwd=-1, lwd.ticks=lwd)
  graphics::axis(4, tcl=tcl, lwd=-1, lwd.ticks=lwd, labels=FALSE)

  graphics::box(lwd=lwd)

  if (is.model) {
#   p <- 1 - stats::pchisq(2 * diff(model$loglik), sum(model$df) - model$idf)
    p <- summary(model)$table[2, "p"]
    if (is.na(p))
      return()
    p <- ifelse(p < 0.001, "p < 0.001", paste("p =", sprintf("%.3f", p)))
    txt <- paste0("Regression, ", p)
    inset <- c(0.02, 0.02 * do.call("/", as.list(graphics::par("pin"))))
    suppressWarnings(graphics::legend("topleft", txt, lty=1, col="#F02311",
                                      xpd=NA, bg="#FFFFFFBB", box.lwd=lwd,
                                      inset=inset))
  }
}
