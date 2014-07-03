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
Reserved columns in the \code{observations} data frame include: \code{"Site_id"}, a unique site identifier; \code{"Site_name"}, a site name; and \code{"Date"}, the observation date.
Observations and standard deviations are located in the subsequent columns and named using unique parameter identifiers.
Data values are checked for a character code in the first digit of each character string.
Values are stripped of their character code and converted to class \code{\link{numeric}}.
The character codes are identified using the following criteria: "<", below recording level; "E", estimated value; "V", contaminated; and "U", undetectable.
A warning is given if the character code is not recognized and its value set to \code{NA}.
Observations with character codes of "V" and "U" are set to \code{NA}.

Required columns in the \code{parameters} data frame include: \code{"Parameter"}, a unique parameter identifier; \code{"Name"}, a parameter name; \code{"Units"}, the parameter units; and \code{"sd"}, the column name in \code{observations} corresponding to the parameters standard deviation values.

Required columns in the \code{detection.limits} data frame include \code{"Date"}, a start date for the detection limit.
Detection limits are located in the subsequent columns and named using unique parameter identifiers.

All components of the \code{observations}, \code{parameters}, and \code{detection.limits} data frames are of class \code{\link{character}}.
}

\value{
Returns an object of class \code{\link{list}} with data frame components corresponding to unique parameter identifiers.
Each data frame has the following components:
\item{Site_id}{numeric; a unique site identifier.}
\item{Site_name}{character; a site name.}
\item{Date}{Date; the observation date.}
\item{code}{factor; character code for observational vlaues.}
\item{conc}{numeric; the observation value.}
\item{sd}{numeric; the standard deviation.}
\item{dl}{numeric; the detection limit.}
\item{t1, t2}{numeric; the starting and ending times for observation intervals, see \code{time} and \code{time2} arguments in \code{\link{Surv}}.}
\item{is.exact}{logical; indicates if the observation is exact.}
\item{is.left}{logical; indicates if the observation is left censored.}
\item{is.interval}{logical; indicates if the observation is interval censored.}
Additional attributes associated with the data frame include: \code{Parameter}, a unique parameter identifier; \code{Name}, a parameter name; and \code{Units}, the parameter units.
}

\author{J.C. Fisher}

\seealso{\code{\link{RunAnalysis}}}

\keyword{file}
