# Example workflow
  library(Trends)

# This workflow requires input files to be located in:
  dir.path <- "D:/WORK/JFisher/Projects/Trend Report"

# Input file paths
  file.data         <- file.path(dir.path, "Data_20120316.txt")
  file.par          <- file.path(dir.path, "Config_Par_20111209.txt")
  file.plots        <- file.path(dir.path, "Config_Plots_20120316.txt")
##file.plots        <- file.path(dir.path, "Config_Plots_Field_20120124.txt")
  file.stats.cens   <- file.path(dir.path, "Config_Cen_20111219.txt")
  file.stats.uncens <- file.path(dir.path, "Config_Uncen_20111209.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_Field_20111117.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_POR_20111117.txt")
##file.stats.uncens <- file.path(dir.path, "Config_Uncen_Field_POR_20111117.txt")

# Read trend data
  d <- ReadTrendData(file.data)

# Plot parameters (not required)
  #ShowParameters(file.par)

# Set graphics device ("pdf", "postscript", "png", "windows")
 #gr.type <- "pdf"
  gr.type <- "postscript"

# Plot trend data

  # Period of record
    sdate <- "01/01/1949"
  ##sdate <- "01/01/1990"
    edate <- "01/01/2010"

  PlotTrendData(d, sdate=sdate, edate=edate, gr.type=gr.type,
                initial.dir=dir.path, file.par=file.par,
                file.plots=file.plots)

# Run statistics

  # Censored data
    stats.tbl.cens <- RunTrendStats(d, is.censored=TRUE, initial.dir=dir.path,
                                    file.par=file.par,
                                    file.stats=file.stats.cens,
                                    write.tbl.out=TRUE, gr.type=gr.type)

  # Uncensored data
    stats.tbl.uncens <- RunTrendStats(d, is.censored=FALSE,
                                      initial.dir=dir.path, file.par=file.par,
                                      file.stats=file.stats.uncens,
                                      write.tbl.out=TRUE, gr.type=gr.type)

