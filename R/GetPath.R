GetPath <- function(type, path=NULL, initial.dir=getwd()) {
# type: "input_data", "config_para", "config_plot", "config_stat",
#       "output_stat", "output_figs"

  titles <- list("input_data" ="Please choose a data file to open",
                 "config_para"="Open configuration file for parameters",
                 "config_plot"="Open configuration file for plots",
                 "config_stat"="Open configuration file for statistics",
                 "output_stat"="Choose file to write output table",
                 "output_figs"=paste("Choose a directory to save",
                                     "graphic files in"))

  errors <- list("input_data" ="Data file does not exist",
                 "config_para"="Parameter configuration file does not exist",
                 "config_plot"="Plot configuration file does not exist",
                 "config_stat"="Statistics configuration file does not exist",
                 "output_stat"="Output file is required to continue",
                 "output_figs"="Output folder is required to continue")

  if (is.null(path)) {
    if (type == "output_figs") {
      path <- paste(tcl("tk_chooseDirectory", initialdir=initial.dir,
                    title=titles[[type]]), collapse=" ")
    } else if (type == "output_stat") {
      path <- paste(tcl("tk_getSaveFile", initialdir=initial.dir,
                        title=titles[[type]], defaultextension="txt",
                        filetypes="{{Text files} {.txt}} {{All files} {*}}"),
                        collapse=" ")
    } else {
      path <- paste(tcl("tk_getOpenFile", initialdir=initial.dir,
                        title=titles[[type]],
                        filetypes="{{Text files} {.txt}} {{All files} {*}}",
                        multiple=FALSE), collapse=" ")
    }
  }

  if (type %in% c("output_figs", "output_stat")) {
    if (length(path) == 0)
      stop(errors[[type]])
  } else {
    if (!file.exists(path))
      stop(errors[[type]])
  }

  path
}
