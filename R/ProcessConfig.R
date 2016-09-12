#' Process Configuration
#'
#' This function processes configuration records for data analysis.
#'
#' @param config data.frame.
#'   See \sQuote{Details} section
#' @param processed.obs list.
#'   See documentation for \code{\link{ProcessObs}} function for details.
#'
#' @details Required columns in the \code{config} data table include:
#'   \code{Site_id}, a unique site identifier;
#'   \code{Site_name}, a local site name; and
#'   \code{Parameter_id}, unique parameter identifier(s).
#'   The \code{config} data table is composed of character-class components.
#'   Multiple parameter identifiers can be specified in a single value of
#'   \code{config$Parameter_id} using a comma separator.
#'   Site and parameter identifiers not found in the \code{processed.obs} list are removed.
#'
#' @return Returns a data.frame object with the following components:
#'   \describe{
#'     \item{Site_id}{unique site identifier}
#'     \item{Site_name}{local site name}
#'     \item{Parameter_id}{unique parameter identifier}
#'     \item{row}{row index in the \code{config} data table.}
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

ProcessConfig <- function(config, processed.obs) {

  ids <- unique(unlist(lapply(processed.obs, function(i) levels(i$Site_id))))
  config <- config[config$Site_id %in% ids, ]

  FUN <- function(i) {
    d <- config[i, , drop=FALSE]
    p <- strsplit(d$Parameter_id, ",")[[1]]
    p <- unique(sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", p, perl=TRUE))
    d$Parameter_id <- NULL
    d <- data.frame(d, Parameter_id=p, row=i, row.names=NULL,
                    stringsAsFactors=FALSE)
    return(d)
  }
  d <- do.call(rbind, lapply(seq_len(nrow(config)), FUN))
  d <- d[d$Parameter_id %in% names(processed.obs), ]
  d <- d[order(as.integer(factor(d$Site_id, levels=unique(d$Site_id)))), ]

  return(d)
}
