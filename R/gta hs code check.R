# Roxygen documentation

#' Check whether your HS codes are consistent with HS 2012.
#'
#' This function checks whether a vector of HS codes is consistent with HS 2012 and returns it as a vector of 6-digit level HS codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed. Values with more than 6 digits will be limited to 6. Please input the codes as integer.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_hs_code_check=function(codes){

  # Load HS names, create 4 and 2 digits columns
  hs.names <- gtalibrary::hs.names
  hs.names$HS12code <- as.factor(sprintf("%06i", hs.names$HS12code))
  hs.names$HS12code.4 <- substr(hs.names$HS12code, 1,4)
  hs.names$HS12code.2 <- substr(hs.names$HS12code, 1,2)

  if (max(nchar(codes)) >= 6) {
    print("Codes with more than 6 figures provided. These will be reduced.")
    codes <- as.numeric(substr(codes, 1,6))
  }

  if (is.numeric(codes)==T) {

    # Convert hs codes in dataframe to numeric
    hs.names$HS12code <- as.numeric(as.character(hs.names$HS12code))
    hs.names$HS12code.2 <- as.numeric(as.character(hs.names$HS12code.2))
    hs.names$HS12code.4 <- as.numeric(as.character(hs.names$HS12code.4))

    #Split to different length sets
    codes.6 <- codes[nchar(codes) >= 5]
    codes.4 <- codes[nchar(codes) == 3 | nchar(codes) == 4]
    codes.2 <- codes[nchar(codes) == 1 | nchar(codes) == 2]

    non.existing <- c()
    if (all(unique(codes.6) %in% hs.names$HS12code) == F){
       non.existing <- append(non.existing, codes.6[! codes.6 %in% hs.names$HS12code])
    }
    if (all(unique(codes.4) %in% hs.names$HS12code.4) == F){
      non.existing <- append(non.existing, codes.4[! codes.4 %in% hs.names$HS12code])
    }
    if (all(unique(codes.2) %in% hs.names$HS12code.2) == F){
      non.existing <- append(non.existing, codes.2[! codes.2 %in% hs.names$HS12code])
    }
    if (length(non.existing) != 0) {
      print(paste0("Non existing values provided: ", paste0(non.existing, collapse = ", ")))
      return(non.existing)

    } else {
      print("All HS codes existing!")
      return(codes)

    }
  } else if (is.numeric(codes)==F) {
      stop("Please input all codes as integer!")
  }

  rm(codes)
}
