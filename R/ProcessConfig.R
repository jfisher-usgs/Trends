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
