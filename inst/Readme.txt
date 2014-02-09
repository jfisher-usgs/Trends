# Workflow

# Set working directory
setwd("E:/WORK/JFisher/Projects/Trend Report 2014")

# Load library
library(Trends)

# Set option to print all warnings
options(warn=1)

# Create output directory under working directory
p <- file.path(getwd(), format(Sys.time(), "%Y%m%d%H%M%S"))
dir.create(path=p)

# Set graphics type
gr.type <- "pdf"

# Read raw data
f.data <- file.path(getwd(), "Data_20140130.tsv")
d <- ReadData(f.data)

# Read geo-referenced site locations
site.locs <- rgdal::readOGR(dsn=getwd(), layer="Site_Locations_20140207")

# Specify name of parameter description file
f.par <- file.path(getwd(), "Config_Par_20140130.tsv")

# Trend plots for non-field parameters and Period-Of-Record (POR)
sdate <- "01/01/1949"
edate <- "01/01/2013"
f.plots <- file.path(getwd(), "Config_Plots_20140130.tsv")
p.figs <- file.path(p, "Data_1949-2012")
dir.create(path=p.figs)
PlotData(d, file.par=f.par, file.plots=f.plots, sdate=sdate, edate=edate,
         gr.type=gr.type, figs.dir=p.figs)
MergePDFs(p.figs)

# Trend plots for non-field parameters and designated time period
sdate <- "01/01/1989"
p.figs <- file.path(p, "Data_1989-2012")
dir.create(path=p.figs)
PlotData(d, file.par=f.par, file.plots=f.plots, sdate=sdate, edate=edate,
         gr.type=gr.type, figs.dir=p.figs)
MergePDFs(p.figs)

# Trend plots for field parameters and the entire POR
sdate <- "01/01/1949"
f.plots <- file.path(getwd(), "Config_Plots_Field_20140130.tsv")
p.figs <- file.path(p, "Data_1949-2012_Field")
dir.create(path=p.figs)
PlotData(d, file.par=f.par, file.plots=f.plots, sdate=sdate, edate=edate,
         gr.type=gr.type, figs.dir=p.figs)
MergePDFs(p.figs)

# Trend plots for field parameters and designated time period
sdate <- "01/01/1989"
p.figs <- file.path(p, "Data_1989-2012_Field")
dir.create(path=p.figs)
PlotData(d, file.par=f.par, file.plots=f.plots, sdate=sdate, edate=edate,
         gr.type=gr.type, figs.dir=p.figs)
MergePDFs(p.figs)

# Statistical plots for censored-non-field parameters and designated time period
f.stats <- file.path(getwd(), "Config_Cen_20140130.tsv")
p.figs <- file.path(p, "Stats_1989-2012_Cen")
dir.create(path=p.figs)
f.out <- file.path(p, paste0(basename(p.figs), ".tsv"))
out <- RunTrendStats(d, file.par=f.par, file.stats=f.stats, is.censored=TRUE,
                     write.tbl.out=TRUE, file.out=f.out, figs.dir=p.figs,
                     gr.type=gr.type)
MergePDFs(p.figs)

# Statistical plots for uncensored-non-field parameters and POR
f.stats <- file.path(getwd(), "Config_Uncen_POR_20140130.tsv")
p.figs <- file.path(p, "Stats_1949-2012_Uncen")
dir.create(path=p.figs)
f.out <- file.path(p, paste0(basename(p.figs), ".tsv"))
out <- RunTrendStats(d, file.par=f.par, file.stats=f.stats, is.censored=FALSE,
                     write.tbl.out=TRUE, file.out=f.out, figs.dir=p.figs,
                     gr.type=gr.type)
MergePDFs(p.figs)

# Statistical plots for uncensored-non-field parameters and designated time period
f.stats <- file.path(getwd(), "Config_Uncen_20140130.tsv")
p.figs <- file.path(p, "Stats_1989-2012_Uncen")
dir.create(path=p.figs)
f.out <- file.path(p, paste0(basename(p.figs), ".tsv"))
out <- RunTrendStats(d, file.par=f.par, file.stats=f.stats, is.censored=FALSE,
                     write.tbl.out=TRUE, file.out=f.out, figs.dir=p.figs,
                     gr.type=gr.type)
MergePDFs(p.figs)

# Statistical plots for uncensored-field parameters and POR
f.stats <- file.path(getwd(), "Config_Uncen_Field_POR_20140130.tsv")
p.figs <- file.path(p, "Stats_1949-2012_Uncen_Field")
dir.create(path=p.figs)
f.out <- file.path(p, paste0(basename(p.figs), ".tsv"))
out <- RunTrendStats(d, file.par=f.par, file.stats=f.stats, is.censored=FALSE,
                     write.tbl.out=TRUE, file.out=f.out, figs.dir=p.figs,
                     gr.type=gr.type)
MergePDFs(p.figs)

# Statistical plots for uncensored-field parameters and designated time period
f.stats <- file.path(getwd(), "Config_Uncen_Field_20140130.tsv")
p.figs <- file.path(p, "Stats_1989-2012_Uncen_Field")
dir.create(path=p.figs)
f.out <- file.path(p, paste0(basename(p.figs), ".tsv"))
out <- RunTrendStats(d, file.par=f.par, file.stats=f.stats, is.censored=FALSE,
                     write.tbl.out=TRUE, file.out=f.out, figs.dir=p.figs,
                     gr.type=gr.type)
MergePDFs(p.figs)
