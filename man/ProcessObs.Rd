\name{ProcessObs}

\alias{ProcessObs}

\title{Process Observations}

\description{
This function processes observations.
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

The \code{observations}, \code{parameters}, and \code{detection.limits} data tables are composed of \code{character-class} components.

Required columns in the \code{observations} data table include: \code{"Site_id"}, a unique site identifier; \code{"Site_name"}, a local site name; and \code{"Date"}, the observation date.
Measured values and their uncertainty (reported as standard deviation) are located in the subsequent columns; a unique parameter identifier is specified for each of these column names.
Measured values are checked for an optional character code in the first digit of each character string value.
Character codes are identified using the following criteria: \code{"<"}, below recording level; \code{"E"}, estimated value; \code{"V"}, contaminated; and \code{"U"}, undetectable.
Values are stripped of their character code and converted to \code{numeric-class}.
A warning is given if the character code is not recognized and its value set to \code{\link{NA}}.
Measured values with character codes of \code{"V"} and \code{"U"} are also set to \code{NA}.

Required columns in the \code{parameters} data table include: \code{"Parameter_id"}, a unique parameter identifier; \code{"Parameter_name"}, the common parameter name; \code{"Units"}, the units associated with the measured parameter values; and \code{"sd"}, a column name in \code{observations} data table where the parameters standard deviation values are located.

A required column in the \code{detection.limits} data table is \code{"Date"}, the date when the detection limit was first implemented.
Detection limit values are located in subsequent columns; a unique parameter identifier is specified for each of these column names.

A measured value is converted to censored data under the following conditions:
(1) the measured value is below the recording level and represented as \emph{left-censored} data; or
(2) there is a standard deviation and detection limit associated with the measured value, therefore, it is represented as \emph{interval-censored} data.
The upper and lower bounds of interval-censored data are calculated by adding and subtracting 3 standard deviations from the measured value, respectively.
Note that interval-censored data with a lower bound less than the detection limit is represented as left-censored data.
For left- and interval-censored data, the upper bound is set to the detection limit when its magnitude is less than the detection limit.
}

\value{
Returns an object of class \code{\link{list}} with \code{data.frame} components corresponding to unique parameter identifiers.
Each data frame has the following components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a local site name.}
\item{Date}{Date; the observation date.}
\item{code}{\code{\link{factor}}; single-digit character code.}
\item{value}{numeric; the measured value.}
\item{sd}{numeric; the standard deviation of the measured value.}
\item{dl}{numeric; the detection limit of the measured value.}
\item{surv}{\code{\link{Surv}}; measurement represented as censored data of \emph{interval} type.}
Additional attributes associated with the returned data frame include: \code{Parameter_id}, a unique parameter identifier; \code{Parameter_name}, a parameter name; and \code{Units}, the parameter units.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{methods}
