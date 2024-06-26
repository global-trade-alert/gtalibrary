% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gta firm related codes last dumps.R
\name{gta_firm_related_codes_last_dumps}
\alias{gta_firm_related_codes_last_dumps}
\title{Create lists of HS and/or CPC codes associated with firms}
\usage{
gta_firm_related_codes_last_dumps(master, firms.df, dump.name.substring)
}
\arguments{
\item{master}{master data frame, through which the type of intervention will be associated with the firm}

\item{firm.df}{firm.df data frame, thanks to which we will obtain the firms that will be uploaded to the database}

\item{dump.name.substring}{Partial or complete string of the name of the data dump in which HS/CPC codes will be searched. For example, if the data dump is USAspending.gov, this parameter could be set to "spending"}
}
\value{
List of HS and/or CPC codes associated with the firms in question.
}
\description{
Returns lists of HS and/or CPC codes of firms uploaded in previous iterations of specific data dumps
}
\references{
www.globaltradealert.org
}
\author{
Global Trade Alert
}
