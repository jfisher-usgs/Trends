## ----setup, include=FALSE, cache=FALSE--------------------------------------------------
try(knitr::opts_chunk$set(dev="pdf", tidy=FALSE, comment="#"), silent=TRUE)
options(warn=1)
options(width=90)
ptime <- proc.time()

## ----eval=FALSE-------------------------------------------------------------------------
#  repos <- c("http://cran.us.r-project.org", "https://jfisher-usgs.github.io/R")
#  install.packages("Trends", repos = repos, dependencies = TRUE, type = "both")

## ---------------------------------------------------------------------------------------
library(Trends)

## ---------------------------------------------------------------------------------------
list.files(path.in <- system.file("extdata", package = "Trends"))

## ---------------------------------------------------------------------------------------
path.out <- file.path(getwd(), paste0("Trends_", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(path = path.out, showWarnings = FALSE)

## ---------------------------------------------------------------------------------------
graphics.type <- "pdf"

## ---------------------------------------------------------------------------------------
read.args <- list(header = TRUE, sep = "\t", colClasses = "character", na.strings = "",
                  fill = TRUE, strip.white = TRUE, comment.char = "", flush = TRUE,
                  stringsAsFactors = FALSE)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Observations.tsv")
observations <- do.call(read.table, c(list(file), read.args))

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Parameters.tsv")
parameters <- do.call(read.table, c(list(file), read.args))

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Detection_Limits.tsv")
detection.limits <- do.call(read.table, c(list(file), read.args))

## ---------------------------------------------------------------------------------------
processed.obs <- ProcessObs(observations, parameters, detection.limits,
                            date.fmt = "%m/%d/%Y")

## ---------------------------------------------------------------------------------------
site.locations <- rgdal::readOGR(path.in, layer = "Site_Locations", verbose = FALSE)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Water_Levels.tsv")
water.levels <- do.call(read.table, c(list(file), read.args))

## ---------------------------------------------------------------------------------------
processed.wl <- ProcessWL(water.levels, date.fmt = "%Y-%m-%d %H:%M")

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Config_NWQL.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path = path.out,
            id = "Stats_NWQL_1989-2012", sdate = "1989-01-01", edate = "2012-12-31",
            site.locations = site.locations, graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Config_Field.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path = path.out,
            id = "Stats_Field_1989-2012", sdate = "1989-01-01", edate = "2012-12-31",
            site.locations = site.locations, graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Config_RAD.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path = path.out,
            id = "Stats_RAD_1981-2012", sdate = "1981-01-01", edate = "2012-12-31",
            site.locations = site.locations, graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Config_VOC.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
RunAnalysis(processed.obs, processed.config, path = path.out,
            id = "Stats_VOC_1987-2012", sdate = "1987-01-01", edate = "2012-12-31",
            site.locations = site.locations, graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
rec <- with(processed.config, Parameter_id == "P32102" & Site_id == "433002113021701")
RunAnalysis(processed.obs, processed.config[rec, ], path = path.out,
            id = "Stats_RWMC_CCl4_2005-2012",sdate = "2005-01-01", edate = "2012-12-31",
            graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
file <- file.path(path.in, "Config_NWQL.tsv")
config <- do.call(read.table, c(list(file), read.args))
processed.config <- ProcessConfig(config, processed.obs)
site.ids <- c("433318112555001", "433456112572001", "433013113024201",
              "433339112565801", "433343112570001")
rec <- with(processed.config, Parameter_id == "CL" & Site_id %in% site.ids)
RunAnalysis(processed.obs, processed.config[rec, ], path = path.out,
            id = "Test",sdate = "1989-01-01", edate = "2012-12-31",
            graphics.type = graphics.type)

## ---------------------------------------------------------------------------------------
RunAnalysis(processed.obs, processed.config[rec, ], path = path.out,
            id = "Test_resWLs",sdate = "1989-01-01", edate = "2012-12-31",
            graphics.type = graphics.type, explanatory.var = processed.wl,
            is.residual = TRUE)

## ---------------------------------------------------------------------------------------
RunAnalysis(processed.obs, processed.config[rec, ], path = path.out,
            id = "Test_Seas",sdate = "1989-01-01", edate = "2012-12-31",
            graphics.type = graphics.type, is.seasonality = TRUE)

## ---------------------------------------------------------------------------------------
RunAnalysis(processed.obs, processed.config[rec, ], path = path.out,
            id = "Test_resWLs_Seas",sdate = "1989-01-01", edate = "2012-12-31",
            graphics.type = graphics.type, is.seasonality = TRUE,
            explanatory.var = processed.wl, is.residual = TRUE)

## ----eval=FALSE-------------------------------------------------------------------------
#  source(system.file("doc", "Trends-process.R", package = "Trends"), echo = TRUE)
#  list.files(path.out, full.names = TRUE, recursive = TRUE)  # path names of output files

## ----echo=FALSE, results="asis"---------------------------------------------------------
print(toLatex(sessionInfo(), locale=FALSE))

