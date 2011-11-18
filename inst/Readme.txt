# Example workflow
  require(Trends)

# This workflow requires input files to be located in:
  dir.path <- "D:/WORK/JFisher/Projects/Trend Report"

# Input file paths
  file.data         <- file.path(dir.path, "Data_20111114.txt")
  file.parameters   <- file.path(dir.path, "Config_Par_20111118.txt")
  file.plots        <- file.path(dir.path, "Config_Plots_20111107.txt")
  file.stats.cens   <- file.path(dir.path, "Config_Cen_20111117.txt")
  file.stats.uncens <- file.path(dir.path, "Config_Uncen_20111117.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_Field_20111117.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_POR_20111117.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_Field_POR_20111117.txt")

# Read trend data
  d <- ReadTrendData(file.data)

# Plot parameters (not required)
  ShowParameters(file.parameters)

# Plot trend data

  # Graphics device ("pdf", "postscript", "png", "windows")
    gr.type <- "pdf"

  # Period of record
    sdate <- "01/01/1950"
  ##sdate <- "01/01/1990"
    edate <- "01/01/2010"

  PlotTrendData(d, sdate=sdate, edate=edate, gr.type=gr.type,
                initial.dir=dir.path, file.parameters=file.parameters,
                file.plots=file.plots)

# Run statistics

  # Graphics device
    gr.type <- "pdf"

  # Time interval which constituent will be averaged ("year", "6 month")
    avg.time <- "year"

  # Censored data
    stats.tbl.cens <- RunTrendStats(d, is.censored=TRUE, initial.dir=dir.path,
                                    file.parameters=file.parameters,
                                    file.stats=file.stats.cens, gr.type=gr.type)

  # Uncensored data
    stats.tbl.uncens <- RunTrendStats(d, is.censored=FALSE,
                                      initial.dir=dir.path,
                                      file.parameters=file.parameters,
                                      file.stats=file.stats.uncens,
                                      avg.time=avg.time, gr.type=gr.type)

