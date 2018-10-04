# Roxygen documentation

#' Find HS codes for product names based on the search engines from EU customs and Eurostat.
#'
#' This function looks for HS codes using the search engines of EU customs and Eurostat. For Eurostat, it only collects the result if it is a direct match (i.e. no further information besides the product name has to be provided.). For the EU customs page, it captures all 6-digit HS codes that the search results for the given product name produce.
#'
#' @param products A vector of product names (in characters).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert



gta_hs_code_finder=function(products){

  library(webdriver)
  library(XML)
  library(stringr)

  find.hs=data.frame(product.name=unique(as.character(products)), hs.code=NA, source=NA)
  find.hs$product.name=as.character(find.hs$product.name)

  pjs <- run_phantomjs()
  remDr=Session$new(port=pjs$port)
  hs.page="https://eurostat.prod.3ceonline.com"

  for(i in 1:nrow(find.hs)){

    remDr$go(hs.page)
    html <- htmlParse(remDr$getSource()[[1]], asText=T)
    if(length(xpathSApply(html,"//textarea[@id='ccce-queryBox']", xmlValue))>0){

      e=remDr$findElement(xpath="//textarea[@id='ccce-queryBox']")
      e$sendKeys(as.character(find.hs$product.name[i]))
      e$sendKeys(key$enter)
      Sys.sleep(2.5)

      html <- htmlParse(remDr$getSource()[[1]], asText=T)
      if(length(xpathSApply(html, "//div[@id='hscode']",xmlValue))>0){
        find.hs$hs.code[i]=as.character(paste(unlist(str_extract_all(xpathSApply(html, "//div[@id='hscode']",xmlValue), "\\d+")),collapse=""))
        find.hs$source[i]="matched via Eurostat HS code finder ('https://eurostat.prod.3ceonline.com')"
        print(find.hs$hs.code[i])
      }else {

        remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", find.hs$product.name[i]),sep=""))

        html <- htmlParse(remDr$getSource()[[1]], asText=T)

        if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6])>0){
          find.hs$hs.code[i]=paste(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6], collapse=",")
          find.hs$source[i]="matched via European customs portal ('https://www.tariffnumber.com', 2013 edition)"
          print(find.hs$hs.code[i])
        }

      }

    }else {

      remDr$go(paste("https://www.tariffnumber.com/2013/", gsub(" ","%20", find.hs$product.name[i]),sep=""))

      html <- htmlParse(remDr$getSource()[[1]], asText=T)

      if(length(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6])>0){
        find.hs$hs.code[i]=paste(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href"))[nchar(gsub("/2013/","",xpathSApply(html, "//a[@class='light text-nowrap']",xmlGetAttr, "href")))==6], collapse=",")
        find.hs$source[i]="matched via European customs portal ('https://www.tariffnumber.com', 2013 edition)"
        print(find.hs$hs.code[i])
      }

    }



    print(i/nrow(find.hs))

  }
  remDr$delete()

  return(find.hs)
  rm(hs.page, find.hs)
}


