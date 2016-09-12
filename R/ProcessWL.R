#' Process Water Levels
#'
#' This function processes water levels for data analysis.
#'
#' @param water.levels data.frame.
#'   See \sQuote{Details} section.
#' @param date.fmt character.
#'   Date format used to convert character strings to Date class.
#'
#' @details Required columns in the \code{water.levels} data table include:
#'   \code{SITE_NO}, a unique site identifier;
#'   \code{LEV_DT}, the measurement date-time;
#'   \code{ALT_VA}, referenced land-surface elevation; and
#'   \code{LEV_VA}, depth below land surface to the water table.
#'   The \code{water.levels} data table is composed of character-class components.
#'
#' @return Returns a data.frame object with the following components:
#'   \describe{
#'     \item{Site_id}{unique site identifier}
#'     \item{Date}{measurement date}
#'     \item{Var}{water-level elevation}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{RunAnalysis}}
#'
#' @keywords methods
#'
#' @export
#'

ProcessWL <- function(water.levels, date.fmt="%Y-%m-%d %H:%M") {

  d <- water.levels

  d$Date <- as.Date(as.POSIXct(d$LEV_DT, format=date.fmt))
  d <- d[!duplicated(paste(d$SITE_NO, d$Date)), ]

  d$Var <- suppressWarnings(as.numeric(d$ALT_VA) - as.numeric(d$LEV_VA))
  d <- d[!is.na(d$Var), ]

  d$Site_id <- as.factor(d$SITE_NO)

  d <- d[, c("Site_id", "Date", "Var")]

  return(d)
}
