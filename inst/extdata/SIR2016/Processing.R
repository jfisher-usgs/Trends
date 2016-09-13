library(Trends)

list.files(path.in <- system.file("extdata", "SIR2016", package="Trends"))
path.out <- file.path(getwd(), paste0("Trends_", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(path=path.out, showWarnings=FALSE)

graphics.type <- "pdf"

read.args <- list(header=TRUE, sep="\t", colClasses="character", na.strings="",
                  fill=TRUE, strip.white=TRUE, comment.char="", flush=TRUE,
                  stringsAsFactors=FALSE)

file <- file.path(path.in, "Observations.tsv")
observations <- do.call(read.table, c(list(file), read.args))

file <- file.path(path.in, "Parameters.tsv")
parameters <- do.call(read.table, c(list(file), read.args))

file <- file.path(path.in, "Detection_Limits.tsv")
detection.limits <- do.call(read.table, c(list(file), read.args))

processed.obs <- ProcessObs(observations, parameters, detection.limits, date.fmt="%m/%d/%Y")


file <- file.path(path.in, "Config_RAD.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path=path.out,
            id = "Stats_RAD_1981-2015", sdate="1981-01-01", edate="2015-12-31",
            site.locations=NULL, graphics.type=graphics.type)


file <- file.path(path.in, "Config_NWQL.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path=path.out,
            id="Stats_NWQL_1989-2015", sdate="1989-01-01", edate="2015-12-31",
            site.locations=NULL, graphics.type=graphics.type)


file <- file.path(path.in, "Config_VOC.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path=path.out,
            id="Stats_VOC_1987-2015", sdate="1987-01-01", edate="2015-12-31",
            site.locations=NULL, graphics.type=graphics.type)
RunAnalysis(processed.obs, processed.config, path=path.out,
            id="Stats_VOC_2005-2015", sdate="2005-01-01", edate="2015-12-31",
            site.locations=NULL, graphics.type=graphics.type)
