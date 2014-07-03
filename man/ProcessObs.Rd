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
\item{observations}{\code{\link{data.frame}}; observational data records, see \sQuote{Details} section.}
\item{parameters}{data.frame; parameter descriptions, see \sQuote{Details} section.}
\item{detection.limits}{data.frame; detection limits (optional), see \sQuote{Details} section.}
\item{date.fmt}{character; the date format used to convert character strings to class \code{\link{Date}}.}
}

\details{
Required columns in the \code{observations} data frame include: \code{"Site_id"}, a unique site identifier; \code{"Site_name"}, a local site name; and \code{"Date"}, the date when the observation was collected or recorded.
Observations and standard deviations are located in the subsequent columns; a unique parameter identifier is specified for each of these columns.
Observed values are checked for a character code in the first digit of each character string.
Values are stripped of their character code and converted to numeric-class.
The character codes are identified using the following criteria: "<", below recording level; "E", estimated value; "V", contaminated; and "U", undetectable.
A warning is given if the character code is not recognized and its value set to \code{NA}.
Observed values with character codes of "V" and "U" are set to \code{NA}.

Required columns in the \code{parameters} data frame include: \code{"Parameter_id"}, a unique parameter identifier; \code{"Parameter_name"}, the common parameter name; \code{"Units"}, the units associated with the parameter=; and \code{"sd"}, a column name in \code{observations} where the parameters standard deviation values are located.

A required column in the \code{detection.limits} data frame is \code{"Date"}, a start date for when the detection limit was implemented.
Values of the detection limit are located in subsequent columns; a unique parameter identifier is specified for each of these columns.

The \code{observations}, \code{parameters}, and \code{detection.limits} data frames are composed of \code{character}-class components.

An observed value is converted to a censored value under the following conditions:
(1) the observed value is below the recording level and represented as \emph{left-censored} data; or
(2) there is a standard deviation and detection limit associated with the observed value, therefore, it is represented as \emph{interval-censored} data.
Note that interval-censored data with bounds located below the detection limit is represented as left-censored data with its upper bound at its detection limit.
}

\value{
Returns an object of class \code{\link{list}} with data frame components corresponding to unique parameter identifiers.
Each data frame has the following components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Date}{Date; the observation date.}
\item{code}{\code{\link{factor}}; single-digit character code.}
\item{value}{numeric; the observed value.}
\item{sd}{numeric; the standard deviation.}
\item{dl}{numeric; the detection limit.}
\item{surv}{\code{\link{Surv}}; censored data of \emph{interval} type.}
Additional attributes associated with the returned data frame include: \code{Parameter_id}, a unique parameter identifier; \code{Parameter_name}, a parameter name; and \code{Units}, the parameter units.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{file}
