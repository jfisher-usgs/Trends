PlotTrendData <- function(d, well.names, sdate=NA, edate=NA,
                          initial.dir=getwd(), file.symbs=NULL, file.plots=NULL,
                          figs.dir=NULL, gr.type="pdf") {
# This function draws plots on the desired device type
# PlotTrendData(d, well.names=c("ANP 6", "ARBOR TEST"), gr.type="windows")

  # Additional functions (subroutines):

  # Trim leading and trailing white space from character string
  trim <- function(s) {
    sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", s, perl=TRUE)
  }


  # Main program:

  require(tcltk)

  # Symbol configuration file
  if (is.null(file.symbs)) {
    txt <- "Open configuration file for symbols"
    file.symbs <- paste(tcl("tk_getOpenFile", initialdir=initial.dir, title=txt,
                            filetypes="{{Text files} {.txt}} {{All files} {*}}",
                            multiple=FALSE), collapse=" ")
  }
  if (!file.exists(file.symbs))
    stop("Symbol file does not exist")

  # Plot configuration file
  if (is.null(file.plots)) {
    txt <- "Open configuration file for plots"
    file.plots <- paste(tcl("tk_getOpenFile", initialdir=initial.dir, title=txt,
                            filetypes="{{Text files} {.txt}} {{All files} {*}}",
                            multiple=FALSE), collapse=" ")
  }
  if (!file.exists(file.plots))
    stop("Plot file does not exist")

  # Output folder for figure files
  if (gr.type != "windows" & is.null(figs.dir)) {
    require(tcltk)
    txt <- paste("Please choose a directory to store", gr.type, "files")
    figs.dir <- paste(tcl("tk_chooseDirectory", initialdir=initial.dir,
                          title=txt), collapse=" ")
    if (length(figs.dir) == 0)
      stop("Output folder is required to continue")
  }

  # Obtain site id(s) to plot
  if (missing(well.names)) {
    key <- unique(d[, c("Site_ID", "Well_name")])
  } else {
    is.well <- well.names %in% levels(d$Well_name)
    if (!all(is.well))
      stop(paste("Well name(s) not in data table: ",
                 paste(well.names[!is.well], collapse=", ")))
    key <- unique(d[which(d$Well_name %in% well.names),
                    c("Site_ID", "Well_name")])
  }
  site.ids <- key$Site_ID
  well.nms <- key$Well_name

  # Convert date-time arguments into POSIXt class
  origin <- as.POSIXct("1920-01-01 00:00:00.0")
  sdate  <- as.POSIXct(sdate, "%m/%d/%Y", tz="MST", origin=origin)
  edate  <- as.POSIXct(edate, "%m/%d/%Y", tz="MST", origin=origin)

  # Read symbol configuration table
  tbl.sym <- read.table(file=file.symbs, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE, comment.char="", row.names=1)
  row.names(tbl.sym) <- make.names(row.names(tbl.sym))

  # Read plot configuration table
  tbl.plt <- read.table(file=file.plots, header=TRUE, sep="\t",
                        stringsAsFactors=FALSE)
  tbl.plt <- tbl.plt[tbl.plt[, "Site_ID"] %in% site.ids, ]

  # Determine constituents
  constituents <- NULL
  for (i in 1:nrow(tbl.plt))
    constituents <- c(constituents,
                      unlist(strsplit(tbl.plt$Constituents[i], ",")))
  constituents <- make.names(trim(unique(constituents)))

  is.in.sym <- constituents %in% row.names(tbl.sym)
  is.in.dat <- constituents %in% names(d)
  if (!all(is.in.sym) | !all(is.in.dat)) {
    idxs <- which(!is.in.sym | !is.in.dat)
    txt <- paste("Inconsistent constituent names among tables:",
                 "table Config_Plots\nProblem is with converted string(s):",
                 paste(constituents[idxs], collapse="; "))
    stop(txt)
  }

  # Reduce size of data table using site id(s) and date-time limits

  d <- d[d[, "Site_ID"] %in% site.ids, ]
  if (!is.na(sdate))
    d <- d[d[, "datetime"] >= sdate, ]
  if (!is.na(edate))
    d <- d[d[, "datetime"] <= edate, ]

  # Loop through site id(s), one page of plots per id

  for (id in site.ids) {

    # Determine plots to draw
    tbl.plt.rows <- which(tbl.plt$Site_ID == id)
    if (length(tbl.plt.rows) == 0)
      next

    # Create a smaller data table that is temporary
    d0 <- d[d[, "Site_ID"] == id, c("datetime", constituents)]

    # Determine x-axis limits
    xlim <- c(sdate, edate)
    xlim.default <- extendrange(d0$datetime)
    if (is.na(xlim[1]))
      xlim[1] <- xlim.default[1]
    if (is.na(xlim[2]))
      xlim[2] <- xlim.default[2]

    # Determine background color for lengend box
    if (gr.type == "postscript")
      leg.box.col <- "#FFFFFF"
    else
      leg.box.col <- "#FFFFFFBB"

    # Open graphics device
    OpenGraphicsDevice(gr.type, well.nms[site.ids == id], figs.dir)
    par(mfrow=c(4, 1), oma=c(5, 5, 5, 5), mar=c(2, 5, 2, 2))

    # Loop through plots, corresponds to rows in the configure plot table

    for (i in seq(along=tbl.plt.rows)) {

      idx <- tbl.plt.rows[i]
      well.name <- tbl.plt[idx, "Well_name"]
      consts <- make.names(trim(unlist(strsplit(tbl.plt$Constituents[idx],
                                                ","))))

      # Determine y-axis limits

      ylim <- c(NA, NA)
      ylim.default <- tryCatch(extendrange(d0[, consts]), warning=function(w) w)
      if (inherits(ylim.default, "simpleWarning")) {
        txt <- paste("No data found for plot:\nSite id: ", id,
                     "; Well name: ", well.name, "; Constituents: ",
                     paste(consts, collapse=", "), "\n", sep="")
        cat(txt)
        next
      }
      ylim[1] <- as.numeric(tbl.plt[idx, "Min"])
      ylim[2] <- as.numeric(tbl.plt[idx, "Max"])
      if (is.na(ylim[1]))
        ylim[1] <- ylim.default[1]
      if (is.na(ylim[2]))
        ylim[2] <- ylim.default[2]

      # Remove constituents with no data
      rm.idxs <- NULL
      for (index in seq(along=consts)) {
        is.no.lim <- all(is.na(d0[, consts[index]]))
        if (is.no.lim)
          rm.idxs <- c(rm.idxs, index)
      }
      if (!is.null(rm.idxs))
        consts <- consts[-rm.idxs]

      # Draw plot

      plot.new()
      plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")

      h <- seq(par("yaxp")[1], par("yaxp")[2], length.out=par("yaxp")[3] + 1)
      v <- pretty(xlim)
      abline(h=h, v=v, col="lightgray")

      lwd <- 0.5 * (96 / (6 * 12))
      tcl.major <- 0.50 / (6 * par("csi"))
      tcl.minor <- 0.25 / (6 * par("csi"))

      axis(2, tcl=tcl.major, lwd=-1, lwd.ticks=lwd)
      axis(4, tcl=tcl.major, lwd=-1, lwd.ticks=lwd, labels=FALSE)

      at.major <- pretty(xlim)
      mult <- 12
      num.at.major <- length(at.major) - 1
      no.match <- TRUE
      while (no.match) {
        at.minor <- pretty(xlim, n=num.at.major * mult)
        if (all(at.major %in% at.minor)) {
          at.minor <- at.minor[!at.minor %in% at.major]
          no.match <- FALSE
        } else if (mult > 100) {
          stop("Problem with minor-tick marks on x-axis")
        } else {
          mult <- mult + 1
        }
      }

      axis.POSIXct(1, at=at.major, tcl=tcl.major, lwd=-1, lwd.ticks=lwd)
      axis.POSIXct(3, at=at.major, tcl=tcl.major, lwd=-1, labels=FALSE,
                   lwd.ticks=lwd)
      axis.POSIXct(1, at=at.minor, tcl=tcl.minor, lwd=-1, lwd.ticks=lwd,
                   labels=FALSE)
      axis.POSIXct(3, at=at.minor, tcl=tcl.minor, lwd=-1, lwd.ticks=lwd,
                   labels=FALSE)

      title(ylab=tbl.plt[idx, "Axis_title"], cex.lab=1, line=3)

      if (i == 1) {
        txt <- paste(well.name, " (", id, ")", sep="")
        mtext(txt, side=3, line=1, col="black")
      }

      box(lwd=lwd)

      # Draw points and legend

      leg <- leg.pch <- leg.col <- leg.bg <- NULL

      for (j in seq(along=consts)) {
        config <- tbl.sym[consts[j], ]
        pch  <- config[, "pch"]
        col  <- config[, "col"]
        bg   <- config[, "bg"]
        name <- config[, "Name"]
        points(d0[, c("datetime", consts[j])], pch=pch, col=col, bg=bg, lwd=lwd)

        leg <- c(leg, name)
        leg.pch <- c(leg.pch, pch)
        leg.col <- c(leg.col, col)
        leg.bg <- c(leg.bg, bg)
      }

      legend(x=grconvertX(0.01, 'npc'), y=grconvertY(0.95, 'npc'),
             leg, pch=leg.pch, col=leg.col, pt.bg=leg.bg,
             xpd=NA, bg=leg.box.col, bty="o", box.lwd=lwd, pt.lwd=lwd)
    }

    # Close graphics device
    if (gr.type != "windows")
      dev.off()
  }
}
