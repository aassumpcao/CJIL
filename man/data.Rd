% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data}
\alias{data}
\title{Data from the North Carolina Judicial Branch Website}
\format{
A tibble containing 13 columns. See below.
described below.
\describe{
\item{period}{Fiscal year when case was filed.}
\item{county}{County Name.}
\item{year}{The year in which var population was recorded.}
\item{recordid}{The unique id for each observation.}
\item{record_timestamp}{Timestamp of last system update.}
\item{category}{Civil, criminal, or infraction case.}
\item{file_type}{
Subcategory of case.
"CVD": Civil District Case
"CVM": Civil Magistrate Case
"CVS": Civil Superior
"SP" : Special Proceeding
"CR" : Criminal District
"CRS": Criminal Superior
"IF" : Infraction
"E"  : Estate
}
\item{jurisdiction}{Who has jurisdiction over case.}
\item{case_type}{Misdemeanors (Traffic)}
\item{case_count}{Case status.}
\item{number_of_cases}{Number of such cases in county.}
\item{ln_number_of_cases}{Log of the number_of_cases.}
\item{popuation}{The population of the county in any given year.}#'
}
}
\source{
\url{data.nccourts.gov} \url{demography.osbm.nc.gov}
}
\usage{
data
}
\description{
This is a data dump from the NC Courts and Demography download API.
}
\keyword{datasets}
