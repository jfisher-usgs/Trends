\name{ProcessWL}

\alias{ProcessWL}

\title{Process Water Levels}

\description{This function processes water levels for data analysis.}

\usage{ProcessWL(water.levels, date.fmt = "\%Y-\%m-\%d \%H:\%M")}

\arguments{
\item{water.levels}{\code{data.frame}; see \sQuote{Details} section.}
\item{date.fmt}{\code{character}; the date format used to convert character strings to \code{Date}-class.}
}

\details{
Required columns in the \code{water.levels} data table include: \code{"SITE_NO"}, a unique site identifier; \code{"LEV_DT"}, the measurement date-time; \code{"ALT_VA"}, referenced land-surface elevation; and \code{"LEV_VA"}, depth below land surface to the water table.
The \code{water.levels} data table is composed of \code{character}-class components.
}

\value{
Returns a \code{data.frame} object with the following components:
\item{Site_id}{\code{factor}; a unique site identifier.}
\item{Date}{\code{Date}; the measurement date.}
\item{Var}{\code{numeric}; the water-level elevation.}
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{methods}
