\name{gtatradecoverage}
\alias{gtatradecoverage}
\title{GTA trade coverage function}
\description{Computes the trade coverages of GTA measures.}
\usage{}
\arguments{
\item{evaluation}{Takes in the values "harmful" or "liberalising". Default: None, it must be defined.}
\item{flow}{This parameter can either be 'export' or 'import'. Default: None, it must be defined.}
\item{importers}{Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. World will be added independent of the ice made. Default: All combinations.}
\item{exporters}{Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. World will be added independent of the ice made. Default: All combinations.}
\item{implementers}{Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).}
\item{products}{Takes in a list of products as numbers (HS2, HS4 or HS6 codes) to filter the sample for products. Codes of different levels can be mixed freely. Default: None.}
\item{sectors}{Takes in a list of sectors as numbers (CPC level 2 or 3) to filter the sample for sectors. Default: None.}
\item{instruments}{Takes in a list of Mast chapters (A, B, CAP, D, E, F, FDI, G, I, L, M, MIG, N, P, TARIFF, X) to filter the sample. Can be set to "include" to include single chapters ng with the aggregated value. Set the first value of the list to "keep" or "remove" in order to include or exclude the chosen values (if not specified, the chosen values will be luded). Default: None.}
\item{inception.range}{Takes a list of two dates, start and end of inception range. Default: c("2009-01-01", infinite).}
\item{implementation.level}{Takes a list of implementation level in numeric form (1 = supra, 2 = nation, 3 = subnat, 4 = SEZ, 5 = IFI, 6 = NFI) to filter the sample. Set the first value the list to "keep" or "remove" in order to include or exclude the chosen values (if not specified, the chosen values will be included). Default: None}
\item{eligible.firms}{Takes a list of eligible firms in numeric form (1 = all, 2 = SME, 3 = firm-specific, 4 =  state-controlled, 5 = state-trading). Set the first value of the list to ep" or "remove" in order to include or exclude the chosen values (if not specified, the chosen values will be included). Default: None.}
\item{in.force}{Takes values TRUE (currenctly in force) or FALSE (not in force anymore), if empty no filtering takes place (in force and not in force anymore).}
\item{rdata}{Takes value TRUE or FALSE. If TRUE, Rdata file will be stored alongside xlsx. Default: FALSE}
\item{output.path}{Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.}
\item{testmode}{Can be set to TRUE or FALSE. If TRUE, sample will be minimized by 99 percent in order to speed up processing. Keep in mind that this will distort results significantly. Default: FALSE.}
}
\details{}
\value{Outputs a table with coverage shares ranging from 2009 to 2018 for each importer, exporter, implementer, instrument combination.}
\references{www.globaltradealert.org}
\author{Global Trade Alert}
\examples{}
