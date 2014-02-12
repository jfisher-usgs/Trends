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

# Read observation data
d <- ReadObservations(file.path(getwd(), "Data_20140212.tsv"))

# Read parameter options file
par.config <- ReadParConfig(file.path(getwd(), "Config_Par_20140212.tsv"))

# Read geo-referenced site locations
site.locs <- ReadSiteLocations(dsn=getwd(), layer="Site_Locations_20140207",
                               verbose=FALSE)

# Plot non-field parameters for Period-Of-Record (POR)
plot.config <- ReadPlotConfig(file.path(getwd(), "Config_Plots_20140212.tsv"))
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1949", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1949-2012"), merge.pdfs=TRUE)

# Plot non-field parameters for designated time period
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1989", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1989-2012"), merge.pdfs=TRUE)

# Plot field parameters for entire POR
plot.config <- ReadPlotConfig(file.path(getwd(), "Config_Plots_Field_20140212.tsv"))
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1949", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1949-2012_Field"), merge.pdfs=TRUE)

# Plot field parameters for designated time period
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1989", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1989-2012_Field"), merge.pdfs=TRUE)

# Trend analysis for censored-non-field parameters and designated time period
plot.config <- ReadPlotConfig(file.path(getwd(), "Config_Cen_20140212.tsv"))
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=TRUE,
                        path.out=file.path(p, "Stats_1989-2012_Cen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-non-field parameters and POR
plot.config <- ReadPlotConfig(file.path(getwd(), "Config_Uncen_20140212.tsv"))
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1949", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1949-2012_Uncen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-non-field parameters and designated time period
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1989-2012_Uncen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-field parameters and POR
plot.config <- ReadPlotConfig(file.path(getwd(), "Config_Uncen_Field_20140212.tsv"))
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1949", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1949-2012_Uncen_Field"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-field parameters and designated time period
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1989-2012_Uncen_Field"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)
