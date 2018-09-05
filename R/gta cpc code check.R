# Roxygen documentation

#' Check whether your CPC codes are consistent with CPC 2.1.
#'
#' This function checks whether a vector of CPC codes is consistent with CPC 2.1 and returns it as a vector of 3-digit level codes.
#'
#' @param codes Supply the CPC codes you want to check. Values with 2 or 3 digits are allowed. If mixing 2nd and 3rd level CPC sector codes, place 3rd level codes in quotes.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_cpc_code_check=function(codes){

  # Load cpc names
  cpc.names <- gtalibrary::cpc.names

  # Check length of longest number in codes, make sure it is not higher than 3
  if (max(nchar(codes)) >= 4) {
    stop("Please provide codes only up to 3 figures")

  } else {
      longest <- max(nchar(codes))
  }

  # Check whether some values occure twice, indicating that there's a problem with leading zeros
  if(length(unique(codes)) != length(codes)){
    stop("Double occurences of cpc code")

  } else {

    # check vector type to decide further processing
    if (is.numeric(codes)==T) {

        # if longest integer has 2 digits, check on cpc level 2
        if (max(nchar(codes)) <= 2) {

          # Check if all cpc codes occur on 2nd level cpc
          if (all(unique(codes) %in% subset(cpc.names, cpc.digit.level == 2)$cpc) == T){
            print("Only 2nd level codes provided: All existing!")

          } else {
            stop("Non existing values provided")
            }

        } else if (max(nchar(codes)) == 3) {

            # Check if all cpc codes occur on 3rd and 2nd level cpc
            if (all(unique(codes) %in% cpc.names$cpc) == T){
              print("3rd and 2nd level codes provided: All existing!")

            } else {
                stop("Non existing values provided")
              }
        }

      # Check whether vector is character, indicating that there are leading zeros cpc 3rd level codes included
    } else if (is.numeric(codes)==F) {

      # Convert cpc.names$cpc to character
      cpc.names$cpc <- as.character(cpc.names$cpc)
      cpc.names.2 <- subset(cpc.names, cpc.digit.level == 2)
      cpc.names.3 <- subset(cpc.names, cpc.digit.level == 3)

      # Add one leading zero for 2nd lvel
      cpc.names.2$cpc <- sprintf("%02s", cpc.names.2$cpc)

      # Add one leading zero for 3rd level
      cpc.names.3$cpc <- sprintf("%03s", cpc.names.3$cpc)

      # rbind 2nd and 3rd level back to cpc.names
      cpc.names <- rbind(cpc.names.2, cpc.names.3)
      rm(cpc.names.2, cpc.names.3)

      # Same for codes provided
      codes.2 <- codes[nchar(codes) <=2]
      codes.3 <- codes[nchar(codes) ==3]

      # Add leading zer for 2nd level
      codes.2 <- sprintf("%02s", codes.2)
      codes.3 <- sprintf("%03s", codes.3)

      # Combine codes.2 and codes.3 to codes
      codes <- append(codes.2, codes.3)
      rm(codes.2, codes.3)

      # Check whether all values exist in cpc.names$cpc
      if (all(unique(codes) %in% cpc.names$cpc) == T){
        print("2nd and 3rd level codes provided: All existing")

      } else {
        stop("Non existing values provided")
        }
    }
  }
  rm(codes)

}
