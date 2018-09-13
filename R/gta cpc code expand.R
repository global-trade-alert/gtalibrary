# Roxygen documentation

#' Expands 2nd level CPC codes to the 3rd level.
#'
#' This function takes in CPC 2nd level codes and outputs all corresponding 3rd level codes in a vector.
#'
#' @param codes Supply the 2nd level CPC codes you want to expand.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_cpc_code_expand=function(codes){

  # Load cpc names
  cpc.names <- gtalibrary::cpc.names

  # Check length of longest number in codes, make sure it is not higher than 3
  if (max(nchar(codes)) >= 3) {
    stop("Please provide codes only up to 2 figures")
  }

  # check vector type to decide further processing
  if (is.numeric(codes)==T) {

    # Expand all numbers to 2 digit factors
    codes <- as.factor(sprintf("%02i",codes))

    #check if all codes exist
    if (all(unique(codes) %in% sprintf("%02i",subset(cpc.names, cpc.digit.level == 2)$cpc) == T)){
      codes <- sprintf("%03i",subset(cpc.names, cpc.digit.level == 3)$cpc)[substr(sprintf("%03i",subset(cpc.names, cpc.digit.level == 3)$cpc),1,2) %in% codes]
      print("Expansion successful. Returning vector of 3rd level CPC codes.")

          return(as.numeric(codes))

        } else {
          non.existing <- codes[! codes %in% sprintf("%02i",subset(cpc.names, cpc.digit.level ==2)$cpc)]
          print(paste0("Non existing values provided: ", paste0(non.existing, collapse = ", ")))

          return(non.existing)

        }

  # Check whether vector is character, indicating that there are leading zeros cpc 3rd level codes included
  } else if (is.numeric(codes)==F) {
      stop("Please input all codes as integer!")
    }

  rm(codes)

}
