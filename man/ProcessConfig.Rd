\name{ProcessConfig}

\alias{ProcessConfig}

\title{Process Configuration}

\description{This function processes configuration records for data analysis.}

\usage{ProcessConfig(config, processed.obs)}

\arguments{
\item{config}{\code{data.frame}; see \sQuote{Details} section.}
\item{processed.obs}{\code{list}; see documentation for \code{\link{ProcessObs}} function for details.}
}

\details{
Required columns in the \code{config} data frame include: \code{"Site_id"}, a unique site identifier; \code{"Site_name"}, a local site name; and \code{"Parameter_id"}, unique parameter identifier(s).
The \code{config} data table is composed of \code{character}-class components.
Multiple parameter identifiers can be specified in a single value of \code{config$Parameter_id} using a comma separator.
Site and parameter identifiers not found in the \code{processed.obs} list are removed.
}

\value{
Returns a \code{data.frame} object with the following components:
\item{Site_id}{\code{numeric}; a unique site identifier.}
\item{Site_name}{\code{character}; a local site name.}
\item{Parameter_id}{\code{character}; a unique parameter identifier.}
\item{row}{\code{integer}; the row index in the \code{config} data table.}
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{methods}
