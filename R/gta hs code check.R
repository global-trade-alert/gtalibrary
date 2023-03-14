# Roxygen documentation

#' Check whether your HS codes are consistent with HS 2012.
#'
#' This function checks whether a vector of HS codes is consistent with HS 2012 and returns it as a vector of 6-digit level HS codes.
#'
#' @param codes Supply the HS codes you want to check. Values with 2 or more digits are allowed. Values with more than 6 digits will be limited to 6. Please input the codes as integer.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

#' @export
gta_hs_code_check=function(codes){

  # Load HS names, create 4 and 2 digits columns
  hs.names <- gtalibrary::hs.names

  hs.codes=c(unique(hs.names$HS12code),
             unique(substr(hs.names$HS12code[(nchar(hs.names$HS12code)%%2)==0],1,2)),
             unique(substr(hs.names$HS12code[(nchar(hs.names$HS12code)%%2)!=0],1,1)),
             unique(substr(hs.names$HS12code[(nchar(hs.names$HS12code)%%2)==0],1,4)),
             unique(substr(hs.names$HS12code[(nchar(hs.names$HS12code)%%2)!=0],1,3)))


  if (is.numeric(codes)==T) {

    if (max(nchar(codes)) >= 7) {
      print(paste0("Codes with more than 6 figures provided. These will be reduced: ", paste0(codes[nchar(codes)>=7], collapse=", ")))

      # taking the first 6 digits from numbers with even digit count, and only the first 5 digits of the others.
      codes <- unique(c(as.numeric(substr(codes[(nchar(codes)%%2)==0], 1,6)),as.numeric(substr(codes[(nchar(codes)%%2)!=0], 1,5))))
    }


    if (sum(as.numeric((codes %in% hs.codes)==F))>0){
      print(paste0("Non existing values provided: ", paste0(codes[(codes %in% hs.codes)==F], collapse = ", ")))


    } else {

      print("Conversion successful. Returning vector of 6-digit HS codes.")

      ## creating a vector of all 6-digit codes

      if(length(codes[nchar(codes)%%2==0])>0 & length(codes[nchar(codes)%%2!=0])>0){
        codes=c(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2==0])[substr(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2==0]),1,nchar(codes[nchar(codes)%%2==0])) %in% codes[nchar(codes)%%2==0]],
                unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2!=0])[substr(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2!=0]),1,nchar(codes[nchar(codes)%%2!=0])) %in% codes[nchar(codes)%%2!=0]])

      } else {

        if(length(codes[nchar(codes)%%2==0])>0 & length(codes[nchar(codes)%%2!=0])==0){
          codes=c(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2==0])[substr(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2==0]),1,nchar(codes[nchar(codes)%%2==0])) %in% codes[nchar(codes)%%2==0]])

        } else {
          codes=c(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2!=0])[substr(unique(hs.names$HS12code[nchar(hs.names$HS12code)%%2!=0]),1,nchar(codes[nchar(codes)%%2!=0])) %in% codes[nchar(codes)%%2!=0]])

        }

      }

      return(codes)

    }
  } else if (is.numeric(codes)==F) {
      stop("Please input all codes as integer!")
  }

  rm(codes)
}

