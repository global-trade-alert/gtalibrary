gta_un_code_vector=function(countries, role=NULL){

  if(is.null(countries)){
    country.un.codes=c(1:9999)
  }

  if(is.null(countries)==F){ ## I know this could have been the "else" of the preceding "if", but find this easier readable

    if(grepl("[A-Za-z]+",paste(countries, collapse=";"))){

      if(sum(as.numeric((tolower(countries) %in% tolower(country.correspondence$name))==F))>0){

        cty.error=paste("'",paste(countries[(tolower(countries) %in% tolower(country.correspondence$name))==F], collapse="; "),"'", sep="")

      } else{
        country.un.codes=unique(country.correspondence$un_code[tolower(country.correspondence$name) %in% tolower(countries)])
      }

    } else{

      if(sum(as.numeric((countries %in% country.correspondence$un_code)==F))>0){
        cty.error=paste("'",paste(countries[(countries %in% country.correspondence$un_code)==F], collapse="; "),"'", sep="")
      } else{
        country.un.codes=countries
      }

    }
  }


  if(exists("cty.error")){
    print(paste("Unkown ",role," country value(s): ", cty.error, ".", sep=""))
  } else {
    return(country.un.codes)
  }
}
