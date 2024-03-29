# Roxygen documentation

#' Parsing TED XML files for GTA relevance
#'
#'
#' @param dom.df Data frame including the DOM
#' @param fx.df Data frame including the FX rates
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#'
gta_ted_parse <- function(dom.df=NULL,
                          fx.df=NULL) {

  dom.df$element.value[nchar(dom.df$element.value)==0]=NA

  output.list<- list("gta.eligible"=F,
                     "parse.successful"=F,
                     "parse.error.msg"=NA)



  #### FIRST TEST: GPA coverage
  ## Is there a GPA reference?
  gpa.references=any(grepl("GPA", dom.df$element.name)|grepl("GPA", dom.df$element.value))

  ## no GPA reference
  if(gpa.references==F){

    output.list<- list("gta.eligible"=F,
                       "parse.successful"=T,
                       "parse.error.msg"="GPA: No reference in the DOM")

    return(output.list)
  }

  ## Got GPA reference, no classifying it
  ##certainly in b/c not covered by GPA
  gta.eligible=any(grepl("NO_CONTRACT_COVERED_GPA", dom.df$element.name))

  ##certainly out b/c covered by GPA or no reference to GPA
  if(any(dom.df$element.name=="CONTRACT_COVERED_GPA")){

    gpa.positions=unique(subset(dom.df, element.name=="CONTRACT_COVERED_GPA")$position)

    if("no" %in% tolower(subset(dom.df, position %in% gpa.positions &  is.attribute==T)$element.value)){
      gta.eligible=T
    } else {

      output.list<- list("gta.eligible"=F,
                         "parse.successful"=T,
                         "parse.error.msg"="GPA: Covered by GPA")


      return(output.list)

    }



  }


  if(gta.eligible==F){

    ## GPA Participation mentioned inside a node
    gpa.participation=any(dom.df$element.name=="RP_REGULATION") & any(grepl("with participation.*?GPA",dom.df$element.value, ignore.case = T))
    if(gpa.participation){
            output.list<- list("gta.eligible"=F,
                               "parse.successful"=T,
                               "parse.error.msg"="GPA: Covered by GPA")

           return(output.list)
    }

     ## Cannot ascertain GPA status
     output.list<- list("gta.eligible"=F,
                        "parse.successful"=F,
                        "parse.error.msg"="GPA: cannot ascertain participation")

     return(output.list)


  }



  #### Second relevance test: CPV values included?
  ## some mentioning of CPV
  got.cpv=any((grepl("CPV", dom.df$element.name)|grepl("CPV", dom.df$element.value)) & is.na(dom.df$element.value)==F)

  if(got.cpv){
    cpv.elements.standard=c("ORIGINAL_CPV","CPV_MAIN","CPV_CODE","CPV_ADDITIONAL","CPV_SUPPLEMENTARY_CODE")

    cpv.elements=unique(dom.df$element.name[grepl("CPV", dom.df$element.name)|grepl("CPV", dom.df$element.value)])

    cpv.easy=any(dom.df$element.name %in% cpv.elements.standard)

    if(cpv.easy){

      cpv.positions=unique(subset(dom.df, element.name %in% cpv.elements.standard)$position)
      cpv.codes=as.numeric(as.character(unique(subset(dom.df, position %in% cpv.positions &
                                                       is.attribute==T &
                                                       is.na(element.value)==F)$element.value)))
      cpv.codes=cpv.codes[!is.na(cpv.codes)]

      if(length(cpv.codes)==0){
        output.list<- list("gta.eligible"=F,
                           "parse.successful"=F,
                           "parse.error.msg"="CPV: Found standard tag, but all codes were non-numeric or inexistent.")

        return(output.list)
      }


    } else {

      output.list<- list("gta.eligible"=F,
                         "parse.successful"=F,
                         "parse.error.msg"="CPV: Could not find a standard tag.")

      return(output.list)

    }

    ## Restricting to those with at least one non-service sector code

    cpv.codes=cpv.codes[cpv.codes<45000000]

    if(length(cpv.codes)==0){
      output.list<- list("gta.eligible"=F,
                         "parse.successful"=T,
                         "parse.error.msg"="CPV: Only service sector codes")

      return(output.list)
    }




  } else {

    output.list<- list("gta.eligible"=F,
                       "parse.successful"=F,
                       "parse.error.msg"="CPV: Did not find CPV values in DOM")

    return(output.list)

  }






  ### THIRD TEST: Contract value
  ted.currency=nrow(subset(dom.df, element.name=="CURRENCY" & is.attribute==T))>0

  if(ted.currency){
    missing.contract.value=T

    ## finding nodes that have a currency tag/attribute, a value tag/attribute and a numeric value
    currency.positions=unique(subset(dom.df, element.name %in% "CURRENCY")$position)
    value.positions=unique(subset(dom.df,
                                  grepl("VAL_OBJECT|VAL_ESTIMATED_TOTAL", element.name, ignore.case = T) |
                                  (grepl("VALUE", element.name, ignore.case = T) & is.attribute==F & is.na(element.value)==F))$position)
    numeric.positions=unique(subset(dom.df, grepl("\\d",element.value))$position)

    ## perfect intersection
    candidate.nodes=intersect(currency.positions,
                              intersect(value.positions, numeric.positions))

    if(length(candidate.nodes)==0){
      ## allowing children positions
      candidate.nodes=intersect(numeric.positions[grepl(paste(currency.positions, collapse="|"), numeric.positions)],
                                numeric.positions[grepl(paste(value.positions, collapse="|"), numeric.positions)])

    }

    value.df=subset(dom.df, grepl(paste(gsub("\\d{2}$","",candidate.nodes), collapse="|"),position) & is.na(element.value)==F)

    ### Looking for values
    if(nrow(value.df)>0){

      date.temp=as.Date(dom.df$element.value[dom.df$element.name=="DATE_PUB"], "%Y%m%d")

      ted.value=data.frame()

      top.pos=unique(value.df$position[nchar(value.df$position)==min(nchar(value.df$position))])

      ## finding the total value

      global.types=unique(subset(dom.df, element.value %in% "GLOBAL")$position)
      if(any(grepl(paste(global.types, collapse="|"), top.pos))){

        top.pos=min(top.pos[grepl(paste(global.types, collapse="|"), top.pos)])

      } else {

        total.tags=c("PROCUREMENT_TOTAL","ESTIMATED_TOTAL","VAL_ESTIMATED_TOTAL","SINGLE_VALUE")

        if(any(total.tags %in% subset(value.df, grepl(paste(top.pos, collapse="|"),position))$element.name)){

          top.pos=min(subset(value.df, element.name %in% total.tags)$position)

        }

      }



      for(tp in top.pos){

        tp.temp=subset(value.df, grepl(tp, position))
        cur.temp=unique(subset(tp.temp, element.name %in% "CURRENCY" & grepl(tp, position))$element.value)


        ## finding a value
        lcu.temp=unique(subset(tp.temp, grepl("FMTVAL",element.name) & grepl(tp, position))$element.value)

        if(length(lcu.temp)==0){
          lcu.temp=unique(subset(tp.temp, grepl("VALUE",element.name) & grepl(tp, position))$element.value)

          if(length(lcu.temp)==0){

            lcu.temp=unique(subset(tp.temp, grepl("text",element.name, ignore.case=T) & grepl(paste(currency.positions, collapse="|"), position))$element.value)
          }

        }

        rm(tp.temp)

        if(length(lcu.temp)==0){
          output.list<- list("gta.eligible"=F,
                             "parse.successful"=F,
                             "parse.error.msg"="CURRENCY: Could not find a value tag")

          return(output.list)

        }

        ted.value=rbind(ted.value,
                        data.frame(date=date.temp,
                                   lcu.value=lcu.temp,
                                   currency=cur.temp,
                                   type=paste(value.df$element.name[value.df$position==tp], collapse=";")))

      }

      if(nrow(ted.value)>0){

        ted.value$lcu.value=gsub("\\s+","",ted.value$lcu.value)
        ted.value=subset(ted.value, grepl("\\D+", gsub(",|\\.","",lcu.value))==F)
        ted.value$lcu.value=gsub("\\.\\d{1,2}",";",ted.value$lcu.value)
        ted.value$lcu.value=gsub(",\\d{1,2}$","",ted.value$lcu.value)
        ted.value$lcu.value=gsub(",","",ted.value$lcu.value)
        ted.value=cSplit(ted.value, which(names(ted.value)=="lcu.value"), sep=";", direction="long")
        ted.value$lcu.value=as.numeric(as.character(ted.value$lcu.value))

        ted.value=subset(ted.value, is.na(lcu.value)==F)

        if(nrow(ted.value)==0){

          output.list<- list("gta.eligible"=F,
                             "parse.successful"=F,
                             "parse.error.msg"="Contract value: Found currency, value tag but only non-numeric content")

          return(output.list)

        }

        missing.contract.value=F
      }

      if(missing.contract.value){
        output.list<- list("gta.eligible"=F,
                           "parse.successful"=F,
                           "parse.error.msg"="Contract value: Found currency, value tag but no numeric content.")

        return(output.list)
      }


    } else {

      output.list<- list("gta.eligible"=F,
                         "parse.successful"=F,
                         "parse.error.msg"="Currency: Found currency tag but no value tag.")

      return(output.list)


    }



  } else {

    output.list<- list("gta.eligible"=F,
                       "parse.successful"=T,
                       "parse.error.msg"="Currency: No tag with 'currency' string found in the DOM.")

    return(output.list)

  }




  ### FOURTH TEST: Contract value above USD 10 million
  ## Value cleaning
  # removing cases with non-digits
  ted.value$date=paste(year(ted.value$date),sprintf("%02i",month(ted.value$date)),sep="-")

  ted.value=merge(ted.value, fx.df, by=c("date", "currency"), all.x=T)

  if(any(is.na(ted.value$lcu.per.usd))){
    missing.value=subset(ted.value, is.na(lcu.per.usd))

    missing.value$lcu.per.usd=NULL
    missing.value$date=paste(strsplit(missing.value$date,"-")[[1]][1],
                             sprintf("%02i",as.numeric(strsplit(missing.value$date,"-")[[1]][2])-1), sep="-")

    missing.value=merge(missing.value, fx.df, by=c("date", "currency"), all.x=T)

    ted.value=rbind(ted.value, missing.value)
    rm(missing.value)

    if(any(is.na(ted.value$lcu.per.usd))){

      output.list<- list("gta.eligible"=F,
                         "parse.successful"=F,
                         "parse.error.msg"="Currency: Missing value when merged with FX data.")

      return(output.list)

    }


  }

  ted.value$usd.value=ted.value$lcu.value/as.numeric(as.character(ted.value$lcu.per.usd))


  ### Adding further variables
  ## Date (annoucement & implementation).
  d.announce=as.Date(dom.df$element.value[dom.df$element.name=="DATE_PUB"], "%Y%m%d")
  if(length(d.announce)==0){
    output.list<- list("gta.eligible"=F,
                       "parse.successful"=F,
                       "parse.error.msg"="No announcement date")

    return(output.list)
  }

  d.implemented=as.Date(dom.df$element.value[dom.df$element.name=="DATE_OPENING_TENDERS"], "%Y-%m-%d")
  if(length(d.implemented)==0){d.implemented=d.announce}

  duration.pos=dom.df$position[dom.df$element.name=="DURATION_TENDER_VALID"]
  duration=character()
  if(length(duration.pos)>0){
    duration=character()

    for(d.pos in duration.pos){
      duration=c(duration,
                 paste(dom.df$element.value[dom.df$position==d.pos], collapse=";"))
    }

    if(length(duration)==0){duration=NA} else {
      duration=paste(unique(duration), collapse=";")
    }

  }
  if(length(duration)==0){duration=NA}

  ## Directive
  if(any(dom.df$element.name=="DIRECTIVE")){
    dir.pos=dom.df$position[dom.df$element.name=="DIRECTIVE"]


  } else {

    if(any(dom.df$element.name=="LEGAL_BASIS")){
      dir.pos=dom.df$position[dom.df$element.name=="LEGAL_BASIS"]

    } else{

      output.list<- list("gta.eligible"=F,
                         "parse.successful"=F,
                         "parse.error.msg"="No directive found")

      return(output.list)

      }

  }


  directive=paste(unique(dom.df$element.value[dom.df$position %in% dir.pos & dom.df$is.attribute==T]  ), collapse=";")

  if(length(directive)==0|nchar(directive)==0){
    output.list<- list("gta.eligible"=F,
                       "parse.successful"=F,
                       "parse.error.msg"="No directive found")

    return(output.list)
  }

  ## Implementing jurisdiction
  ij.pos=dom.df$position[dom.df$element.name=="ISO_COUNTRY"]
  ij=paste(unique(dom.df$element.value[dom.df$position %in% ij.pos & dom.df$is.attribute==T]), collapse=";")

  if(length(ij)==0){
    output.list<- list("gta.eligible"=F,
                       "parse.successful"=F,
                       "parse.error.msg"="No implementer found")

    return(output.list)
  }

  ## Tender implementation
  ## issuing authority
  tender.authority=paste(dom.df$element.value[dom.df$element.name=="AA_AUTHORITY_TYPE"], collapse=";")

  tender.nature=paste(dom.df$element.value[dom.df$element.name=="NC_CONTRACT_NATURE"], collapse=";")

  issuer.pos=min(dom.df$position[dom.df$element.name=="AA_NAME"])

  tender.issuer=paste(unique(dom.df$element.value[dom.df$position %in% issuer.pos & dom.df$element.name=="AA_NAME"]), collapse=";")
  tender.issuer.lang=paste(unique(dom.df$element.value[dom.df$position %in% issuer.pos & dom.df$element.name=="LG"]), collapse=";")

  if(length(tender.issuer)==0){
    output.list<- list("gta.eligible"=F,
                       "parse.successful"=F,
                       "parse.error.msg"="No tender issuer found")

    return(output.list)
  }


  tender.doc.type=dom.df$element.value[dom.df$element.name=="TD_DOCUMENT_TYPE"]
  if(length(tender.doc.type)==0){tender.doc.type=NA}

  if(length(dom.df$position[dom.df$element.name=="LG" & dom.df$element.value=="EN"])>0){
    ti.pos=dom.df$position[dom.df$element.name=="LG" & dom.df$element.value=="EN"]
  } else {
    orig.lg=dom.df$element.value[dom.df$element.name=="LG_ORIG"]
    ti.pos=dom.df$position[dom.df$element.name=="LG" & dom.df$element.value==ti.pos]
  }

  if(length(ti.pos)>0){
    tender.town=character()
    tender.content=character()

    for(tp in ti.pos){

      tender.town=c(tender.town,
                    dom.df$element.value[dom.df$element.name=="TI_TOWN" & grepl(tp, dom.df$position)])
      tender.content=c(tender.content,
                       dom.df$element.value[dom.df$element.name=="TI_TEXT" & grepl(tp, dom.df$position)])


    }

    if(length(tender.town)==0){tender.town=NA} else {
      tender.town=paste(unique(tender.town), collapse=";")
    }

    if(length(tender.content)==0){tender.content=NA}else {
      tender.content=paste(unique(tender.content), collapse=";")
    }

  }



  ## beneficiary

  contractor.country=character()
  contractor.name=character()
  contractor.town=character()

  con.pos=dom.df$position[dom.df$element.name=="CONTRACTOR"]
  if(length(con.pos)>0){

    for(i in 1:length(con.pos)){

      for(c in con.pos){


        contractor.name=c(contractor.name,
                          dom.df$element.value[dom.df$element.name=="OFFICIALNAME" & grepl(c, dom.df$position)])

        contractor.town=c(contractor.town,
                          dom.df$element.value[dom.df$element.name=="TOWN" & grepl(c, dom.df$position)])

        cty.pos=dom.df$position[dom.df$element.name=="COUNTRY" &
                                    grepl(c, dom.df$position)]

        contractor.country=c(contractor.country, unique(c(contractor.country, dom.df$element.value[dom.df$is.attribute==T &
                                                                                                       grepl(cty.pos, dom.df$position)])))
      }

    }

    contractor.name=paste(unique(contractor.name), collapse=";")
    contractor.town=paste(unique(contractor.town), collapse=";")
    contractor.country=paste(unique(contractor.country), collapse=";")
  }

  if(length(contractor.name)==0){contractor.name=NA}
  if(length(contractor.town)==0){contractor.town=NA}
  if(length(contractor.country)==0){contractor.country=NA}

  ted.base=data.frame(directive=directive,
                      implementing.jurisdiction=ij,
                      date.announced=d.announce,
                      date.implemented=d.implemented,
                      duration=duration,
                      issuer.name=tender.issuer,
                      issuer.name.language=tender.issuer.lang,
                      tender.authority=tender.authority,
                      tender.town=tender.town,
                      tender.nature=tender.nature,
                      tender.content=tender.content,
                      tender.doc.type=tender.doc.type,
                      contractor.name=contractor.name,
                      contractor.town=contractor.town,
                      contractor.country=contractor.country,
                      stringsAsFactors = F)


  ### Parse complete
  output.list<- list("gta.eligible"=T,
                     "parse.successful"=T,
                     "parse.error.msg"=NA,
                     "ted.base"=ted.base,
                     "ted.value"=ted.value,
                     "ted.cpv"=cpv.codes)

  return(output.list)

}
