.onAttach <- function(lib, pkg) {
  if (interactive()) {
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    packageStartupMessage(pkg, ": version: ", ver)

    s <- "  Type 'citation(\"Trends\")' for citing this R package in publications."
    packageStartupMessage(s)
  }

  invisible()
}
