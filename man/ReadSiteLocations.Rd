\name{ReadSiteLocations}

\alias{ReadSiteLocations}

\title{Read Site Location Shapefile}

\description{This function reads the geo-referenced observation site locations.}

\usage{ReadSiteLocations(...)}

\arguments{
\item{...}{arguments to pass to the \code{\link{readOGR}} function.}
}

\format{
Required fields include the site identifier and local site name, read from the
first and second fields, respectively.
}

\value{
Returns a \code{SpatialPointsDataFrame} with data frame components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
}

\author{J.C. Fisher}

\seealso{\code{\link{readOGR}}}

\examples{
obj <- ReadSiteLocations(dsn = system.file("extdata", package = "Trends"),
                         layer = "ex.site.locations")
str(obj)
}

\keyword{IO}
