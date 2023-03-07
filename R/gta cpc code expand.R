# Roxygen documentation

#' Expands 2-digit level CPC codes into 3-digit level CPC codes.
#'
#' This function takes in 2-digit level CPC codes and returns all corresponding 3-digit level CPC codes in a vector.
#'
#' @param codes Supply the 2-digit level CPC codes you want to expand.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
#'

#' ## necessary ? can be returned with check as well...
gta_cpc_code_expand <- function(codes, as_list = FALSE, message = TRUE) {
    # Load cpc names
    ## use these, remove hs values, take the 3 digit codes, make 2 digit codes out of it --> then put in two columns and use one for the key, other for the value :) (mabye rename parameters in function)
    # cpc.names <- gtalibrary::cpc_to_hs
    # make use of the existing CPC_HS_conversion table and modify it for this purpose
    cpc_expansion <- readRDS("c:\\Users\\sveng\\OneDrive\\Dokumente\\GitHub\\GTA\\gtalibrary\\data\\cpc_to_hs.rda")
    cpc_expansion <- cpc_expansion |>
        dplyr::mutate(HS_2012 = stringr::str_sub(CPC_21, 1, 2)) |>
        dplyr::rename(cpc_2_digit = HS_2012, cpc_3_digit = CPC_21) |>
        dplyr::distinct()

    # Check length of longest number in codes, make sure it is not higher than 3
    # gta_logical_check(codes, \(x) nchar(codes) %in% c(1, 2))
    stringr::str_pad(codes, width = 2, side = "left", pad = "0")

    # use cpc_to_hs_CPP function as it does the same
    conversion <- cpc_to_hs_CPP(cpc_codes = cpc_expansion$cpc_2_digit, hs_2012_codes = cpc_expansion$cpc_3_digit, codes = codes)
    ###########
    ##########
    #########
    ########
    #######
    ######
    #####
    ####
    ###
    ##
    #
    return(conversion)
}

microbenchmark::microbenchmark(
    times = 1,
    gta_cpc_code_expand(codes = rep("01", 100000))
)
