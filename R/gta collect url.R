# Roxygen documentation

#' Collects a URL and stores it locally using file name. Websites are printed into PDF, URLS to files are downloaded.
#'
#' @return The complete file name of the collected url (incl. file format).
#' @references www.globaltradealert.org
#' @author GTA


# Function infos and parameters  --------------------------------------------


gta_collect_url = function(url=NULL,
                          file.name=NULL,
                          store.path=NULL,
                          add.timestamp=T,
                          update.file.name=T,
                          js.path="setup/rasterize.js",
                          phantom.path="~/bin/phantomjs"){

  library(stringr)

  if(is.null(store.path)){
    file.path=file.name

  } else {

    file.path=paste0(gsub("/$","",store.path),"/", file.name)

  }

  if(add.timestamp){

    t.stamp=paste0(" - ", gsub("\\D","-",Sys.time()))

  } else {
    t.stamp=""
  }


  ## check whether URL leads to file
  file.types=c("doc", "pdf","xls","txt","csv","rdata")
  is.file=grepl(paste(file.types, collapse="|"),str_extract(url,"\\.[A-Za-z]{1,5}$"), ignore.case = T)


  if(is.file){
    ## download file

    file.suffix=str_extract(url,"\\.[A-Za-z]{1,5}$")

    GET(url, write_disk(paste0(file.path, t.stamp, file.suffix), overwrite=TRUE))



  } else {

    ## print into PDF

    file.suffix=".pdf"


    cmd=paste(phantom.path,
              js.path,
              paste0("'",url,"'"),
              paste0("'",file.path, t.stamp, file.suffix,"'"),
              "'A1'")

    system(cmd)

  }




  # return file name
  if(update.file.name){

    new.file.name=paste0(file.path, t.stamp, file.suffix)


  } else {
    new.file.name=file.name
  }
  return(new.file.name)

}
