#' Process Observations
#'
#' This function processes measurements from observation sites in a monitoring network.
#'
#' @param observations data.frame.
#'   Observed data records, see \sQuote{Details} section.
#' @param parameters data.frame.
#'   Parameter descriptions, see \sQuote{Details} section.
#' @param detection.limits data.frame.
#'   Detection limits, see \sQuote{Details} section.
#' @param date.fmt character.
#'   Date format used to convert character strings to Date class.
#'
#' @details The \code{observations}, \code{parameters}, and \code{detection.limits}
#'   data tables are composed of character-class components.
#'
#'   Required columns in the \code{observations} data table include:
#'     \code{Site_id}, a unique site identifier;
#'     \code{Site_name}, a local site name; and
#'     \code{Date}, the measurement date.
#'   Measured values are checked for an optional character code in the first digit of each character string value.
#'   Character codes are identified using the following criteria:
#'     \emph{<}, below reporting level;
#'     \emph{E}, estimated value;
#'     \emph{V}, contaminated; and
#'     \emph{U}, undetectable.
#'   Values are stripped of their character code and converted to numeric class.
#'   A warning is given if the character code is not recognized and its value set to NA.
#'   Measured values with character codes of \emph{V} and \emph{U} are also set to NA.
#'
#'   Required columns in the \code{parameters} data table include:
#'     \code{Parameter_id}, a unique parameter identifier;
#'     \code{Parameter_name}, the common parameter name;
#'     \code{Units}, the units associated with the measured parameter values; and
#'     \code{sd}, a column name in \code{observations} data table where the parameters standard deviation values are located.
#'
#'   A required column in the \code{detection.limits} data table is \code{Date},
#'   the date when the detection limit was first implemented.
#'   Detection limit values are located in subsequent columns;
#'   a unique parameter identifier is specified for each of these column names.
#'
#'   A measured value is converted to censored data under the following conditions:
#'     (1) the measured value is below the reporting level and represented as \emph{left-censored} data; or
#'     (2) there is a standard deviation and detection limit associated with the measured value,
#'         therefore, it is represented as \emph{interval-censored} data.
#'   The upper and lower bounds of interval-censored data are calculated by
#'   adding and subtracting three standard deviations from the measured value, respectively.
#'   Interval-censored data with a lower or upper bound less than the detection limit is represented as left-censored data.
#'   For left-censored data, the upper bound is set to the detection limit when its magnitude is less than the detection limit.
#'
#' @return Returns an object of class list with data.frame components corresponding to unique parameter identifiers.
#'   Each data table has the following components:
#'   \describe{
#'     \item{Site_id}{unique site identifier}
#'     \item{Site_name}{local site name}
#'     \item{Date}{observation date}
#'     \item{code}{single-digit character code}
#'     \item{value}{ measured value}
#'     \item{sd}{standard deviation of the measured value.}
#'     \item{dl}{detection limit of the measured value.}
#'     \item{surv}{observation as censored or uncensored data of \emph{interval} type.}
#'   }
#'   Additional attributes associated with the returned data frame include:
#'     \code{Parameter_id}, unique parameter identifier;
#'     \code{Parameter_name}, parameter name; and
#'     \code{Units}, parameter units.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{RunAnalysis}}
#'
#' @keywords methods
#'
#' @export
#'

ProcessObs <- function(observations, parameters, detection.limits=NULL,
                       date.fmt="%Y-%m-%d") {

  observations$Date <- as.Date(observations$Date, format=date.fmt)
  observations <- observations[!is.na(observations$Date), ]

  par.names <- make.names(parameters$Parameter_id)
  par.names <- sort(par.names[par.names %in% colnames(observations)])

  detection.limits$Date <- as.Date(detection.limits$Date, format=date.fmt)
  detection.limits[, -1] <- apply(detection.limits[, -1], 2, as.numeric)

  lst <- list()
  for (i in seq_along(par.names)) {
    nam <- par.names[i]
    is.rec <- !is.na(observations[[nam]]) & observations[[nam]] != ""
    if (all(!is.rec))
      next

    d <- observations[is.rec, c("Site_id", "Site_name", "Date")]
    d <- data.frame(d, code=NA, value=NA, sd=NA, dl=NA)
    d$Site_id   <- as.factor(d$Site_id)
    d$Site_name <- as.factor(d$Site_name)

    d$value <- as.character(observations[is.rec, par.names[i]])
    d$code <- substr(d$value, 1, 1)
    d$code[!d$code %in% c("<", "E", "V", "U")] <- ""
    d$code <- as.factor(d$code)
    is.code <- d$code != ""
    d$value[is.code] <- substr(d$value[is.code], 2, nchar(d$value[is.code]))
    d$value <- as.numeric(d$value)
    d$value[d$code %in% c("V", "U")] <- NA

    p <- parameters[match(nam, make.names(parameters$Parameter_id)), , drop=TRUE]
    sd.col <- p$sd
    idx <- ifelse(!is.na(sd.col), match(sd.col, colnames(observations)), NA)
    d$sd <- if (is.na(idx)) NA else as.numeric(observations[is.rec, idx])

    if (nam %in% colnames(detection.limits)) {
      dl <- detection.limits[!is.na(detection.limits[[nam]]), c("Date", nam)]
      for (id in levels(d$Site_id)) {
        idxs <- which(d$Site_id == id)
        breaks <- findInterval(as.numeric(dl$Date), as.numeric(d$Date[idxs]))
        breaks <- unique(c(breaks, length(idxs) + 1L))
        d$dl[idxs] <- dl[as.integer(cut(seq_along(idxs), breaks)), nam]
      }
    }

    t1 <- t2 <- rep(NA, nrow(d))

    is.left <- d$code %in% "<"
    is.exact <- !is.left & (is.na(d$sd) | is.na(d$dl))
    t1[is.exact] <- d$value[is.exact]
    t2[is.exact] <- d$value[is.exact]
    t2[is.left]  <- d$value[is.left]
    lower.value <- d$value - 3 * d$sd
    upper.value <- d$value + 3 * d$sd

    are.left <- which(!is.exact & upper.value <= d$dl)
    t2[are.left] <- d$dl[are.left]
    are.left <- which(!is.exact & lower.value <= d$dl & upper.value > d$dl)
    t2[are.left] <- upper.value[are.left]

    are.interval <- which(!is.exact & lower.value > d$dl)
    t1[are.interval] <- lower.value[are.interval]
    t2[are.interval] <- upper.value[are.interval]

    t2[which(t2 <= 0)] <- NA  # TODO: zero is invalid for 'lognormal' distribution
    t1[which(t1 <= 0 | is.na(t2))] <- NA

    d$surv <- Surv(time=t1, time2=t2, type="interval2")

    d <- d[order(d$Site_name, d$Date), ]
    vars <- c("Parameter_id", "Parameter_name", "Units")
    attributes(d) <- c(attributes(d), p[vars])

    lst[[p$Parameter_id]] <- d
  }

  return(lst)
}
