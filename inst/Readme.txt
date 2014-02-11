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
f.obs <- file.path(getwd(), "Data_20140211.tsv")
d <- ReadObservations(f.obs)

# Read parameter options file
f.par <- file.path(getwd(), "Config_Par_20140211.tsv")
par.config <- ReadParConfig(f.par)

# Read geo-referenced site locations
layer <- "Site_Locations_20140207"
site.locs <- ReadSiteLocations(dsn=getwd(), layer=layer, verbose=FALSE)

# Plot non-field parameters for Period-Of-Record (POR)
sdate <- "01/01/1949"
f.plot <- file.path(getwd(), "Config_Plots_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Data_1949-2012")
dir.create(path=path.out)
PlotObservations(d, par.config=par.config, plot.config=plot.config, sdate=sdate,
                 edate="01/01/2013", gr.type=gr.type, path.out=path.out)
MergePDFs(path.out)

# Plot non-field parameters for designated time period
sdate <- "01/01/1989"
path.out <- file.path(p, "Data_1989-2012")
dir.create(path=path.out)
PlotObservations(d, par.config=par.config, plot.config=plot.config, sdate=sdate,
                 edate="01/01/2013", gr.type=gr.type, path.out=path.out)
MergePDFs(path.out)

# Plot field parameters for entire POR
sdate <- "01/01/1949"
f.plot <- file.path(getwd(), "Config_Plots_Field_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Data_1949-2012_Field")
dir.create(path=path.out)
PlotObservations(d, par.config=par.config, plot.config=plot.config, sdate=sdate,
                 edate="01/01/2013", gr.type=gr.type, path.out=path.out)
MergePDFs(path.out)

# Plot field parameters for designated time period
sdate <- "01/01/1989"
path.out <- file.path(p, "Data_1989-2012_Field")
dir.create(path=path.out)
PlotObservations(d, par.config=par.config, plot.config=plot.config, sdate=sdate,
                 edate="01/01/2013", gr.type=gr.type, path.out=path.out)
MergePDFs(path.out)

# Trend analysis for censored-non-field parameters and designated time period
sdate <- "01/01/1989"
f.plot <- file.path(getwd(), "Config_Cen_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Stats_1989-2012_Cen")
dir.create(path=path.out)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate=sdate, edate="01/01/2013", is.censored=TRUE,
                        path.out=path.out, gr.type=gr.type, site.locs=site.locs)
MergePDFs(path.out)

# Trend analysis for uncensored-non-field parameters and POR
sdate <- "01/01/1949"
f.plot <- file.path(getwd(), "Config_Uncen_POR_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Stats_1949-2012_Uncen")
dir.create(path=path.out)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate=sdate, edate="01/01/2013", is.censored=FALSE,
                        path.out=path.out, gr.type=gr.type, site.locs=site.locs)
MergePDFs(path.out)

# Trend analysis for uncensored-non-field parameters and designated time period
sdate <- "01/01/1989"
f.plot <- file.path(getwd(), "Config_Uncen_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Stats_1989-2012_Uncen")
dir.create(path=path.out)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate=sdate, edate="01/01/2013", is.censored=FALSE,
                        path.out=path.out, gr.type=gr.type, site.locs=site.locs)
MergePDFs(path.out)

# Trend analysis for uncensored-field parameters and POR
sdate <- "01/01/1949"
f.plot <- file.path(getwd(), "Config_Uncen_Field_POR_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Stats_1949-2012_Uncen_Field")
dir.create(path=path.out)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate=sdate, edate="01/01/2013", is.censored=FALSE,
                        path.out=path.out, gr.type=gr.type, site.locs=site.locs)
MergePDFs(path.out)

# Trend analysis for uncensored-field parameters and designated time period
sdate <- "01/01/1989"
f.plot <- file.path(getwd(), "Config_Uncen_Field_20140211.tsv")
plot.config <- ReadPlotConfig(f.plot)
path.out <- file.path(p, "Stats_1989-2012_Uncen_Field")
dir.create(path=path.out)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate=sdate, edate="01/01/2013", is.censored=FALSE,
                        path.out=path.out, gr.type=gr.type, site.locs=site.locs)
MergePDFs(path.out)
