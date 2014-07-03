\name{ProcessConfig}

\alias{ProcessConfig}

\title{Process Configuration}

\description{This function processes a configuration table.}

\usage{ProcessConfig(config, processed.obs)}

\arguments{
\item{config}{\code{\link{data.frame}}; see \sQuote{Details} section.}
\item{processed.obs}{\code{\link{list}}; see \code{\link{ProcessObs}} for details.}
}

\details{
Required column names in the \code{config} data frame include: \code{"Site_id"}, \code{"Site_name"}, and \code{"Parameter_id"}.
Variables in the data frame are of character-class.
Multiple parameter identifiers can be specified in a single record of \code{config$Parameter_id} using a comma separator.
Site and parameter identifiers not found in \code{processed.obs} are removed.
}

\value{
Returns a data frame with the following components:
\item{Site_id}{numeric; a site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameter_id}{character; a parameter identifier.}
\item{rec}{integer; record (row) index in \code{config}.}
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{IO}
