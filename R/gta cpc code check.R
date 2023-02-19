# Roxygen documentation

#' Check whether your CPC codes are consistent with CPC 2.1.
#'
#' This function checks whether a vector of CPC codes is consistent with CPC 2.1 and returns it as a vector of 3-digit level codes.
#'
#' @param codes Supply the CPC codes you want to check. Only 3-digit level CPC codes are allowed. If mixing 2-digit and 3-digit CPC sector codes, place 2-digit codes inside the gta_cpc_code_expand function.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_cpc_code_check <- function(codes) {
  # Load cpc names
  cpc.names <- gtalibrary::cpc.names

  # Check length of longest number in codes, make sure it is not higher than 3
  if (max(nchar(codes)) >= 4) {
    stop("Please provide codes only up to 3 figures")
  }
  # check vector type to decide further processing
  if (is.numeric(codes)) {
    # Expand all codes to 3 digits, to take care of leading zeros
    codes <- as.factor(sprintf("%03i", codes))

    # Check if all cpc codes occur on 2nd level cpc
    if (all(unique(codes) %in% sprintf("%03i", subset(cpc.names, cpc.digit.level == 3)$cpc) == T)) {
      codes <- sprintf("%03i", subset(cpc.names, cpc.digit.level == 3)$cpc)[substr(sprintf("%03i", subset(cpc.names, cpc.digit.level == 3)$cpc), 1, 3) %in% codes]
      print("Conversion successful. Returning vector of 3-digit CPC codes.")

      return(as.numeric(codes))
    } else {
      non.existing <- codes[!codes %in% sprintf("%03i", subset(cpc.names, cpc.digit.level == 3)$cpc)]
      print(paste0("Non-existing values provided: ", paste0(non.existing, collapse = ", ")))

      return(non.existing)
    }
    # Check whether vector is character, indicating that there are leading zeros cpc 3rd level codes included
  } else if (!is.numeric(codes)) {
    stop("Please input all codes as integer!")
  }
  rm(codes)
}

devtools::install_github("global-trade-alert/gtalibrary@hs_functions")

sessionInfo()
