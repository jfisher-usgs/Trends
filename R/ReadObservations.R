ReadObservations <- function(file) {

  if (missing(file) | !file.exists(file))
    stop("Data file missing or does not exist")

  # Read data from file, format determined by R with no factor conversion
  # Character code at front of string:
  #   Less than: "<" = 1
  #   Estimated value: "E" = 2
  #   Sample contaminated: "V" = 3
  #   Undetectable: "U" = 4
  #   No character code: 0
  d <- read.table(file=file, header=TRUE, sep="\t", fill=TRUE, strip.white=TRUE,
                  blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE,
                  stringsAsFactors=FALSE)

  # Get dimensions of data table (m X n)
  m <- nrow(d)
  n <- ncol(d)

  # For character class data columns (variables):
  #   - Covert to numeric after accounting for character codes
  #   - Exclude first four variables: Site_id, Site_name, Dates, Times

  col.idxs <- which(!names(d) %in% c("Site_id", "Site_name", "Dates", "Times"))
  for (j in col.idxs) {

    if (!is.character(d[, j]))
      next

    # Translate character codes to integer codes
    str.1 <- substr(d[, j], 1L, 1L)
    if (any(c("<", "E", "V", "U") %in% str.1)) {

      # Intialize integer code with 0s
      code <- rep(0L, m)

      # Translate character codes to integer codes
      code[str.1 == "<"] <- 1L
      code[str.1 == "E"] <- 2L
      code[str.1 == "V"] <- 3L
      code[str.1 == "U"] <- 4L

      # Strip character code from variable values with "<" or "E" codes
      is.code <- code == 1L | code == 2L
      d[is.code, j] <- substr(d[is.code, j], 2L, nchar(d[is.code, j]))

      # Remove variable values with "V" or "U" codes
      is.code <- code == 3L | code == 4L
      d[is.code, j] <- NA

      # Add integer codes to data table
      add.col.name <- paste(names(d)[j], "code", sep="_")
      d[, add.col.name] <- code
    }

    # Specify empty strings with NA
    d[str.1 == "", j] <- NA

    # Warn if NAs introduced by coercion, user needs to address these in the
    # data text file.
    ans <- tryCatch(as.numeric(d[, j]), warning=function(w) w)
    if (inherits(ans, "simpleWarning")) {
      for (i in 1L:m) {
        ans <- tryCatch(as.numeric(d[i, j]), warning=function(w) w)
        if (inherits(ans, "simpleWarning")) {
          txt <- paste0(gettext(ans),
                        "Row index: ", i, ", Column index: ", j, "\n",
                        "Column name: ", names(d)[j], ", String: ", d[i, j],
                        "\n")
          cat(txt)
        }
      }
    }

    # Make final conversion to numeric and supress warnings
    d[, j] <- suppressWarnings(as.numeric(d[, j]))
  }

  # Convert site ids and names to factor class
  d$Site_id   <- as.factor(d$Site_id)
  d$Site_name <- as.factor(d$Site_name)

  # Add "Datetime" variable, calendar date and time of class POSIXct
  # Missing time values are replaced with 12:00 pm or 1200
  d$Times[is.na(d$Times)] <- 1200
  dt.string <- paste(d$Dates, sprintf("%04d", d$Times), sep=" ")
  d$Datetime <- as.POSIXct(dt.string, format="%m/%d/%Y %H%M", tz="MST",
                           origin=as.POSIXct("1920-01-01 00:00:00.0"))

  is.dt.na <- is.na(d$Datetime)
  if (any(is.dt.na)) {
    cat("\nWarning in Datetime: NAs introduced by coercion\n")
    cat(paste("Row: ", which(is.dt.na), ", String: ", dt.string[is.dt.na],
              "\n", collapse="\n", sep=""))
  }

  # Reorganize components in the data table
  d <- d[, !names(d) %in% c("Dates", "Times")]
  is.id <- names(d) %in% c("Site_id", "Site_name", "Datetime")
  d <- d[, c(which(is.id), which(!is.id))]

  # Return data table
  return(d)
}
