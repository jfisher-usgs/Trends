\name{ProcessObs}

\alias{ProcessObs}

\title{Process Observations}

\description{
This function processes observation data.
}

\usage{
ProcessObs(observations, parameters, detection.limits = NULL, date.fmt = "\%Y-\%m-\%d")
}

\arguments{
\item{observations}{data.frame; observational data records, see \sQuote{Details} section.}
\item{parameters}{data.frame; parameter descriptions, see \sQuote{Details} section.}
\item{detection.limits}{data.frame; detection limits (optional), see \sQuote{Details} section.}
\item{date.fmt}{character; the date format used to convert date character strings to class \code{\link{Date}}.}
}


\details{
Required column names in the \code{observations} data frame include: \code{"Site_id"}, \code{"Site_name"}, and \code{"Date"}.
All other column names (with the exception of \code{"Time"}) correspond to unique parameter identifiers.
Observational values are checked for a character code in the first digit of the character string.
These codes are stripped of their codes and converted to class \code{\link{numeric}}.
The character codes are identified using the following criteria: "<", below recording level; "E", estimated value; "V", contaminated; and "U", undetectable.
A warning is given if the character code is not recognized and its value set to \code{NA}.
Observations with character codes of "V" and "U" are also set to \code{NA}.

Required column names in the \code{parameters} data frame include: \code{"Parameter"}, \code{"Name"}, \code{"Units"}, and \code{"sd"}.
The \code{sd} variable gives the column name in \code{observations} corresponding to the parameters standard deviation values.

Required column names in the \code{detection.limits} data frame include \code{"Date"}, a start date for the detection limit.
All other column names correspond to unique parameter identifiers.

All variables in the \code{observations}, \code{parameters}, and \code{detection.limits} data frames are of class \code{\link{character}}.
}

\value{
Returns an object of class \code{\link{list}} with data frame components corresponding to unique parameter identifiers.
Each data frame has the following components:
\item{Site_name}{character; a local site name.}
\item{Site_id}{numeric; a site identifier.}
\item{Date}{Date; the observation date.}
\item{code}{factor; character code for observational vlaues.}
\item{conc}{numeric; the observation value.}
\item{sd}{numeric; the standard deviation.}
\item{dl}{numeric; the detection limit.}
\item{t1, t2}{numeric; the starting and ending times of the observation interval, see \code{time} and \code{time2} arguments in \code{\link{Surv}}.}
\item{is.exact}{logical; indicates if the observation is exact.}
\item{is.left}{logical; indicates if the observation is left censored.}
\item{is.interval}{logical; indicates if the observation is interval censored.}
Additional attributes associated with the data frame include: \code{Parameter}, the parameter identifier; \code{Name}, the parameter name; and \code{Units}, the parameter units.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{file}
