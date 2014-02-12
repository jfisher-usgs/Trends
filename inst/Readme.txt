# Workflow

# Load library
library(Trends)

# Set option to print all warnings
options(warn=1)

# Set path name for output directory
p <- file.path(getwd(), paste0("Trends_", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(path=p)

# Set graphics type
gr.type <- "pdf"

# Read observation data
f <- system.file("extdata/SIR2014/Data.tsv", package="Trends")
d <- ReadObservations(f)

# Read parameter options file
f <- system.file("extdata/SIR2014/Config_Par.tsv", package="Trends")
par.config <- ReadParConfig(f)

# Read geo-referenced site locations
dsn <- system.file("extdata/SIR2014", package="Trends")
site.locs <- ReadSiteLocations(dsn, layer="Site_Locations", verbose=FALSE)

# Plot non-field parameters for Period-Of-Record (POR)
f <- system.file("extdata/SIR2014/Config_Plots.tsv", package="Trends")
plot.config <- ReadPlotConfig(f)
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1960", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1960-2012"), merge.pdfs=TRUE)

# Plot non-field parameters for designated time period
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1989", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1989-2012"), merge.pdfs=TRUE)

# Plot field parameters for entire POR
f <- system.file("extdata/SIR2014/Config_Plots_Field.tsv", package="Trends")
plot.config <- ReadPlotConfig(f)
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1960", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1960-2012_Field"), merge.pdfs=TRUE)

# Plot field parameters for designated time period
PlotObservations(d, par.config=par.config, plot.config=plot.config,
                 sdate="01/01/1989", edate="01/01/2013", gr.type=gr.type,
                 path.out=file.path(p, "Data_1989-2012_Field"), merge.pdfs=TRUE)

# Trend analysis for censored-non-field parameters and designated time period
f <- system.file("extdata/SIR2014/Config_Cen.tsv", package="Trends")
plot.config <- ReadPlotConfig(f)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=TRUE,
                        path.out=file.path(p, "Stats_1989-2012_Cen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-non-field parameters and POR
f <- system.file("extdata/SIR2014/Config_Uncen.tsv", package="Trends")
plot.config <- ReadPlotConfig(f)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1960", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1960-2012_Uncen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-non-field parameters and designated time period
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1989-2012_Uncen"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-field parameters and POR
f <- system.file("extdata/SIR2014/Config_Uncen_Field.tsv", package="Trends")
plot.config <- ReadPlotConfig(f)
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1960", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1960-2012_Uncen_Field"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)

# Trend analysis for uncensored-field parameters and designated time period
out <- RunTrendAnalysis(d, par.config=par.config, plot.config=plot.config,
                        sdate="01/01/1989", edate="01/01/2013", is.censored=FALSE,
                        path.out=file.path(p, "Stats_1989-2012_Uncen_Field"),
                        gr.type=gr.type, site.locs=site.locs, merge.pdfs=TRUE)
