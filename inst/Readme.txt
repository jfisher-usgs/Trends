# Trend package example workflow
  require(Trends)

# This workflow requires input files to be located in:
  dir.path <- "D:/WORK/JFisher/Projects/Trend Report"

# Input file paths

  file.data         <- file.path(dir.path, "Data_20111101.txt")
  file.symbs        <- file.path(dir.path, "Config_Symbols_20111102.txt")
  file.plots        <- file.path(dir.path, "Config_Plots_20111101.txt")
  file.stats.uncens <- file.path(dir.path, "Stats_Uncensored_20111027.txt")
  file.stats.cens   <- file.path(dir.path, "Stats_Censored_20111027.txt")

# Read trend data
  d <- ReadTrendData(file.data)

# Plot symbols (not required)
  ShowSymbols(file.symbs)

# Plot trend data

  # Graphics device ("pdf", "postscript", "png", "windows")
    gr.type <- "pdf"

  # Period of record
    sdate <- "01/01/1950" # or sdate <- "01/01/1990"
    edate <- "01/01/2010"

  PlotTrendData(d, sdate=sdate, edate=edate, gr.type=gr.type,
                initial.dir=dir.path, file.symbs=file.symbs,
                file.plots=file.plots)

# Run statistics

  # Graphics device
    gr.type <- "pdf"

  # Time interval which constituent will be averaged ("year", "6 month")
    avg.time <- "year"

  # Uncensored data
    stats.tbl.uncens <- RunStats(d, is.censored=FALSE, initial.dir=dir.path,
                                 file.stats=file.stats.uncens, avg.time=avg.time,
                                 gr.type=gr.type)

  # Censored data
    stats.tbl.cens <- RunStats(d, is.censored=TRUE, initial.dir=dir.path,
                               file.stats=file.stats.cens, avg.time=avg.time,
                               gr.type=gr.type)

