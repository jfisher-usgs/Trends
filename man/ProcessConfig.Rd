\name{ProcessConfig}

\alias{ProcessConfig}

\title{Process Configuration}

\description{This functions processes a configuration table.}

\usage{ProcessConfig(config, processed.obs)}

\arguments{
\item{config}{something...}
\item{processed.obs}{something...}
}

\format{
The \code{config} table requires the following fields:
\code{"Site_id"}, \code{"Site_name"}, \code{"Parameters"}, and
\code{"Axis_title"}. The first line in the data table must contain variable
names. See \sQuote{Value} for field descriptions.
}

\value{
Returns a data frame with components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Parameters}{character; comma separated unique parameter identifiers.}
\item{Axis_title}{factor; plot label for the y-axis (allows for missing
  values).}
}

\author{J.C. Fisher}

\seealso{\code{\link{read.table}}}

\examples{
print("notyet")
}

\keyword{IO}
