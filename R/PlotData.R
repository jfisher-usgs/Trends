PlotData <- function(d, site.names, file.par, file.plots, sdate=NA, edate=NA,
                     figs.dir=getwd(), gr.type="pdf") {

  # Additional functions:

  # Trim leading and trailing white space from character string
  trim <- function(s) {
    sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", s, perl=TRUE)
  }

  # Open and close graphics device
  GrDev <- function(site, plot.count) {
    if (gr.type != "windows")
      graphics.off()
    site <- paste0(site, "_", LETTERS[((4L + plot.count) - 1L) %/% 4L])
    OpenGraphicsDevice(figs.dir, site, gr.type)
  }


  # Main program:

  # Obtain site id(s) to plot
  if (missing(site.names)) {
    key <- unique(d[, c("Site_id", "Site_name")])
  } else {
    is.site <- site.names %in% levels(d$Site_name)
    if (!all(is.site))
      stop(paste("Site name(s) not in data table: ",
                 paste(site.names[!is.site], collapse=", ")))
    key <- unique(d[which(d$Site_name %in% site.names),
                    c("Site_id", "Site_name")])
  }
  site.ids <- key$Site_id
  site.nms <- key$Site_name

  # Convert date-time arguments into POSIXt class
  origin <- as.POSIXct("1920-01-01 00:00:00.0")
  sdate  <- as.POSIXct(sdate, "%m/%d/%Y", tz="MST", origin=origin)
  edate  <- as.POSIXct(edate, "%m/%d/%Y", tz="MST", origin=origin)

  # Read parameter configuration table
  tbl.par <- read.table(file=file.par, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE, comment.char="", row.names=1)
  row.names(tbl.par) <- make.names(row.names(tbl.par))

  # Read plot configuration table
  tbl.plt <- read.table(file=file.plots, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE)
  is.valid.site.id <- tbl.plt$Site_id %in% site.ids
  if (!all(is.valid.site.id)) {
    ids <- tbl.plt[!is.valid.site.id, c("Site_id", "Site_name"), drop=FALSE]
    msg <- paste(paste0("id: ", ids$Site_id, ", name: ", ids$Site_name),
                 collapse="\n")
    warning("Ids in plots configuration file do not match data:\n", msg, "\n")
  }
  tbl.plt <- tbl.plt[is.valid.site.id, ]

  # Determine parameters
  parameters <- NULL
  for (i in 1:nrow(tbl.plt))
    parameters <- c(parameters,
                    unlist(strsplit(tbl.plt$Parameters[i], ",")))
  parameters <- make.names(trim(unique(parameters)))

  is.in.par <- parameters %in% row.names(tbl.par)
  is.in.dat <- parameters %in% names(d)
  if (!all(is.in.par) | !all(is.in.dat)) {
    idxs <- which(!is.in.par | !is.in.dat)
    txt <- paste("Inconsistent parameter names among tables:",
                 "table Config_Plots\nProblem is with converted string(s):",
                 paste(parameters[idxs], collapse="; "))
    stop(txt)
  }

  # Reduce size of data table using site id(s) and date-time limits
  d <- d[d[, "Site_id"] %in% site.ids, ]
  if (!is.na(sdate))
    d <- d[d[, "Datetime"] >= sdate, ]
  if (!is.na(edate))
    d <- d[d[, "Datetime"] <= edate, ]

  # Loop through site id(s)

  for (id in site.ids) {

    # Initialize plot count
    plot.count <- 0L

    # Site name
    site <- site.nms[site.ids == id]

    # Determine plots to draw
    tbl.plt.rows <- which(tbl.plt$Site_id == id)
    if (length(tbl.plt.rows) == 0)
      next

    # Create a smaller data table that is temporary
    d0 <- d[d[, "Site_id"] == id, c("Datetime", parameters)]

    # Set x-axis limits
    xlim <- c(sdate, edate)

    # Determine background color for lengend box
    if (gr.type == "postscript")
      leg.box.col <- "#FFFFFF"
    else
      leg.box.col <- "#FFFFFFBB"

    # Loop through parameter sets

    for (i in seq_along(tbl.plt.rows)) {

      idx <- tbl.plt.rows[i]

      # Create a smaller data table that is temporary
      p.names <- make.names(trim(unlist(strsplit(tbl.plt$Parameters[idx],
                                                 ","))))
      d1 <- d0[, c("Datetime", p.names)]

      # Remove parameters with no data
      rm.idxs <- NULL
      for (index in 1:ncol(d1)) {
        is.no.lim <- all(is.na(d1[, index]))
        if (is.no.lim)
          rm.idxs <- c(rm.idxs, index)
      }
      if (!is.null(rm.idxs))
        d1 <- d1[, -rm.idxs]
      if (!inherits(d1, "data.frame") || ncol(d1) < 2) {
        cat(paste("No data found for plot:\nSite id: ", id,
                  "; Site name: ", site, "; Parameters: ",
                  paste0(p.names, collapse=", "), "\n"))
        next
      }

      # Set y-axis limits
      ylim <- as.numeric(tbl.plt[idx, c("Min", "Max")])

      # Draw plot
      plot.count <- plot.count + 1L
      if (((4L + plot.count) - 1L) %% 4L == 0L) {
        GrDev(site, plot.count)
        main <- paste0(site, " (", id, ")")
      } else {
        main <- NULL
      }
      DrawPlot(d1, tbl.par, xlim=xlim, ylim=ylim, main=main,
               ylab=tbl.plt[idx, "Axis_title"], leg.box.col=leg.box.col)
    }

    # Close graphics device
    if (gr.type != "windows")
      graphics.off()
  }
}
