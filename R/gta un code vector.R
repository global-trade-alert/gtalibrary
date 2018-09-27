# Roxygen documentation

#' Create a vector of UN country codes
#'
#' Returns a vector of codes based on country or group names.
#'
#' @param countries Vector with either country names, country group names or UN codes. Do not mix UN codes with country or group names. Please use function also for UN codes to ensure the ones you supply are consistent with the ones used by the GTA.
#' @param role This parameter tailors the error message e.g. "Unkown ROLE country value ...". Only useful if command is run for more than one set of countries.
#'
#' @return A vector of UN country codes that is consistent with those used by the GTA.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_un_code_vector=function(countries, role=NULL){

  if(is.null(countries)){
    country.un.codes=c(1:9999)
  }

  if(is.null(countries)==F){ ## I know this could have been the "else" of the preceding "if", but find this easier readable


    ## preparation: a correspondence between country/group names and UN codes
    country.correspondence=gtalibrary::country.correspondence
    country.names=gtalibrary::country.names

    ## Checking parameters

    if (all(tolower(countries) %in% c(tolower(unique(country.correspondence$name)), unique(country.correspondence$un_code)))==F) {
      cty.error = countries[(tolower(countries) %in% c(tolower(unique(country.correspondence$name)), unique(country.correspondence$un_code)))==F]
      stop(paste("Unkown ",role," country value(s): ", paste(cty.error, collapse =", ")))
    }

    ## Checking & converting codes
    country.un.codes=NULL
    if(grepl("[A-Za-z]+",paste(countries, collapse=";"))){

        country.un.codes=c(country.un.codes, unique(country.correspondence$un_code[tolower(country.correspondence$name) %in% tolower(countries)]))

    }

    if(sum(as.numeric((tolower(countries) %in% tolower(country.correspondence$name))==F))>0){

      country.un.codes = unique(c(country.un.codes, countries[(countries %in% country.correspondence$un_code)==T]))

    }

    return(country.un.codes)

    rm(countries, role, country.un.codes, cty.error)

  }
}
