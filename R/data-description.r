#' hs.vintage conversion matrix
#'
#' hs.vintages contains the conversion matrix between different HS versions
#' The underlying conversion matrices are obtained from [LINK] and allow you
#' to convert HS codes from one vintage year to another.
#' @usage This data set is used in the function `gta_hs_vintage_converter()` to convert
#' HS codes into 2012 vintage.
"hs.vintages"

#' eligible firm information
#'
#' elig.firms contains the valid names for XX in the XX data frame
"elig.firms"
head(as_tibble(gtalibrary::elig.firms))

#' TITLE
#'
#'
"imp.level"

######## hs.groups is now in hs.codes --> Plus this contains HS07 and not HS2012 --> Could be removed
nrow(as_tibble(gtalibrary::hs.groups))
view(gtalibrary::hs.groups)

#' hs.code information
#'
#' hs.codes contains the name of hs.codes and information on which group they belong to
#' @usage
"hs.codes"
head(as_tibble(gtalibrary::hs.codes))

#' country.names
#'
#' This data set contains the name as well as the un_code of each country as it is used in the gta database.
#' Additionally, a logical indicator for the membership of the country in various organizations or classifications (eg. is.eu, is.wto, is.wb.low)
#' is provided
"country.names"

#' cpc_hs_matrix
#' @details
#' # The data set was obtained using the following code:
#' cpc_hs_matrix <- readr::read_csv("https://unstats.un.org/unsd/classifications/Econ/tables/CPC/CPCv21_HS12/cpc21-hs2012.txt")
#' cpc_hs_matrix <- cpc_hs_matrix |>
#'     dplyr::select(CPC21code, HS12code) |>
#'     dplyr::mutate(
#'         HS12code = str_remove(HS12code, "\\."),
#'         CPC21code = str_extract(CPC21code, pattern = "\\d{3}")
#'     ) |>
#'     dplyr::rename(HS_2012 = HS12code, CPC_21 = CPC21code) |>
#'     dplyr::distinct()
"cpc_hs_matrix"
