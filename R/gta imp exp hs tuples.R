# Roxygen documentation

#' GTA Importer-Exporter-Product tuples
#'
#' Generates the importer-exporter-product tuples for GTA interventions.
#'
#' @param master.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param master.data.frame Specify if the master.path is a data frame in the current environment. Default is FALSE. If you supply a data frame, the first three columns need to be (1) intervention ID, (2) implementation date, (3) removal date. In that order.
#' @param replica.path Location of the database replica. Default is 'data/database replica/database replica - parts - base.Rdata'.
#' @param df.name Set the name of the generated result data frame. Default is 'master.tuple'.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is 'parameter.tuple'.
#'
#' @return Output is two data frames. First data frame includes the importer-exporter-product relationships for the given interventions. Second data frame states parameter choices.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_imp_exp_hs_tuples <- function(
  master.path="data/master_plus.Rdata",
  master.data.frame=FALSE,
  replica.path="data/database replica/database replica - parts - base.Rdata",
  df.name="master.tuple",
  pc.name="parameter.tuple"
) {
  # initialising
  library(data.table)
  library(splitstackshape)
  parameter.choices=data.frame(parameter=character(), choice=character())

  ## data file
  if(master.data.frame){
    eval(parse(text=paste("master=", master.path, sep="")))

    if(ncol(master)<3){stop("Please supply a data frame with at least three columns (intervention ID, implementation and removal date).")}

    if(ncol(master)==3){
      names(master)=c("intervention.id", "date.implemented", "date.removed")
    }else{
      names(master)=c("intervention.id", "date.implemented", "date.removed", names(master[,4:ncol(master)]))
    }
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice=paste("Data frame '",master.path,"' from the global environment.", sep="")))

  }else{
    if(master.path=="online"){
      print("Downloading the latest copy of the GTA dataset.The file is deleted after loading the data into your environment.")
      download.file("https://www.dropbox.com/s/78kpe232p2b36ze/GTA%20full%20data%20export.Rdata?dl=1","GTA data.Rdata")
      load("GTA data.Rdata")
      unlink("GTA data.Rdata")
      parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice="Downloaded latest copy"))
    } else{
      load(master.path)
      parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice=paste("Local copy from '",master.path,"'.", sep="")))
    }
  }

  ## loading tuple file
  keep.inv=ls()
  load(replica.path)
  rm(list = setdiff(ls(), c(keep.inv, "gta_tuple")))
  gta_tuple=gta_tuple[,c("intervention_id","un_code_implementer","un_code_distorted","un_code_affected", "affected_products")]
  names(gta_tuple)=c("intervention.id","un.ij","un.dm","un.aj", "affected.product")
  gta_tuple=subset(gta_tuple, intervention.id %in% master$intervention.id)
  parameter.choices=rbind(parameter.choices, data.frame(parameter="Data base replica source:", choice=paste("Local copy from '",replica.path,"'.", sep="")))


  ## transform IJ/DM/AJ so that we have i.un and a.un as importers and exporters
  master.tuple=data.frame(intervention.id=numeric(),i.un=numeric(), a.un=numeric(), t.un=numeric(), affected.product=numeric())

  if("inward" %in% master$affected.flow){
    inward=subset(master, affected.flow=="inward")[,c("intervention.id","i.un", "a.un", "affected.product")]
    inward$t.un=inward$i.un
    inward=cSplit(inward, which(names(inward)=="affected.product"), direction="long", sep=",")
    master.tuple=rbind(master.tuple, inward)
    rm(inward)
  }
  if("outward" %in% master$affected.flow){
    outward=subset(master, affected.flow=="outward")[,c("intervention.id","i.un", "a.un", "affected.product")]
    setnames(outward, "i.un","t.un")
    outward$i.un=outward$a.un
    outward$a.un=outward$t.un
    outward=cSplit(outward, which(names(outward)=="affected.product"), direction="long", sep=",")
    master.tuple=rbind(master.tuple, outward)
    rm(outward)
  }

  if("outward subsidy" %in% master$affected.flow){
    os=subset(master, affected.flow=="outward subsidy")[,c("intervention.id","i.un", "a.un", "affected.product")]
    setnames(os, "i.un","t.un")
    os=cSplit(os, which(names(os)=="affected.product"), direction="long", sep=",")
    gt.os=subset(gta_tuple, intervention.id %in% os$intervention.id)
    names(gt.os)=c("intervention.id","t.un","i.un","a.un", "affected.product")

    os=merge(os, gt.os, by=c("intervention.id","t.un","a.un", "affected.product"), all.x=T)
    os=subset(os, is.na(i.un)==F)
    master.tuple=rbind(master.tuple, os)
    rm(gt.os, os)
  }

  rm(gta_tuple)

  master.tuple=subset(master.tuple, is.na(i.un)==F & is.na(a.un)==F & is.na(t.un)==F & is.na(affected.product)==F)

  ## create global DFs
  eval(parse(text=paste(df.name, "<<-master.tuple", sep="")))
  eval(parse(text=paste(pc.name, "<<-parameter.choices", sep="")))

}
