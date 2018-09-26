# Roxygen documentation

#' GTA trade coverage function
#'
#' Computes the trade coverages of GTA measures.
#'
#' @param data.path.coverage Specifies where the GTA data file is located (Default: 'data/database replica'). Set to 'online' to download the latest copy.
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
#' @param affected.flow Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param importers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for importers in the sample. World will be added independent of the choice made. Default: All combinations.
#' @param exporters Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for exporters in the sample. World will be added independent of the choice made. Default: All combinations.
#' @param implementers Takes in a list of country names, UN codes or country groups (g7, g20, eu28, ldc, au) to filter for implementers in the sample. Default: World (as in implemented by one).
#' @param implementer.role Bilateral trade flows can be affected by multiple actors. Specify which actor's interventions you want to include. There are three roles: importer, exporter and 3rd country. Combinations are permissible. Default: c('importer','3rd country').
#' @param rdata Takes value TRUE or FALSE. If TRUE, Rdata file will be stored alongside xlsx. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.
#' @param testmode Can be set to TRUE or FALSE. If TRUE, sample will be minimized by 99 percent in order to speed up processing. Keep in mind that this will distort results significantly. Default: FALSE.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param in.force.today Specify whether you want to focus on interventions in force today ('TRUE') or no longer in force today ('FALSE'). Default is 'any'.
#' @param intervention.type Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param mast.chapter Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
#' @param keep.mast Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated MAST chapter ID.
#' @param implementation.level Specify the government level responsible for the intervention.  Default is 'any'. Permissible values are 'supranational', 'national', 'subnational', 'IFI', 'NFI' or combinations thereof. IFI and NFI refer to government-owned financial institutions that are owned by one ('NFI') or more ('IFI') governments.
#' @param keep.level Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementation levels.
#' @param eligible.firms Concentrate the analysis on interventions that are targeted at certain subsets. Default is 'any'. Permissible values are 'all', 'firm-specific', 'SMEs', 'state-controlled','state trading enterprise' or combinations thereof.
#' @param keep.firms Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated firm subsets.
#' @param cpc.sectors Provide a vector of CPC codes that you are interested in (version 2.1, any digit level).
#' @param keep.cpc Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated CPC codes.
#' @param hs.codes Provide a vector of HS codes that you are interested in (2012 vintage, any digit level).
#' @param keep.hs Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated HS codes.
#' @param intervention.ids Provide a vector of intervention IDs.
#' @param keep.intervention Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
#'
#' @return Outputs a table with coverage shares ranging from 2009 to 2018 for each importer, exporter, implementer, instrument combination.
#' @references www.globaltradealert.org
#' @author Global Trade Alert


# Function infos and parameters  --------------------------------------------

gta_trade_coverage <- function(
  data.path="data/master_plus.Rdata",
  data.path.coverage="data/trade coverage/trade coverage base file.Rdata",
  gta.evaluation= NULL,
  affected.flow = NULL,
  importers = NULL,
  exporters = NULL,
  implementers = NULL,
  implementer.role = NULL,
  announcement.period = NULL,
  implementation.period = NULL,
  revocation.period = NULL,
  in.force.today = NULL,
  intervention.type = NULL,
  keep.type = NULL,
  mast.chapter = NULL,
  keep.mast = NULL,
  implementation.level = NULL,
  keep.level = NULL,
  eligible.firms = NULL,
  keep.firms = NULL,
  cpc.sectors = NULL,
  keep.cpc = NULL,
  hs.codes = NULL,
  keep.hs = NULL,
  intervention.ids = NULL,
  keep.intervention = NULL,
  lag.adjustment=NULL,
  rdata = FALSE,
  output.path = NULL,
  testmode = FALSE) {

  # For testing

  # data.path="data/master_plus.Rdata"
  # data.path.coverage="data/trade coverage/trade coverage base file.Rdata"
  # gta.evaluation= NULL
  # affected.flow = NULL
  # importers = NULL
  # exporters = NULL
  # implementers = NULL
  # implementer.role = NULL
  # announcement.period = NULL
  # implementation.period = NULL
  # revocation.period = NULL
  # in.force.today = NULL
  # intervention.type = NULL
  # keep.type = NULL
  # mast.chapter = NULL
  # keep.mast = NULL
  # implementation.level = NULL
  # keep.level = NULL
  # eligible.firms = NULL
  # keep.firms = NULL
  # cpc.sectors = NULL
  # keep.cpc = NULL
  # hs.codes = NULL
  # keep.hs = NULL
  # intervention.ids = NULL
  # keep.intervention = NULL
  # lag.adjustment=NULL
  # rdata = FALSE
  # output.path = NULL
  # testmode = FALSE

  # importers = c("United States of America", "Germany","g20","EU-28")
  # exporters = NULL
  # affected.flow ="inward"
  # implementers = NULL
  # implementer.role = NULL
  # rdata = FALSE

  # Initialising Function ---------------------------------------------------

  # load libraries
  library("xlsx")
  library("splitstackshape")
  library("data.table")

  ######## Feed data slicer

  data_slicer <- gta_data_slicer(data.path=data.path,
                                 gta.evaluation= gta.evaluation,
                                 affected.flow = affected.flow,
                                 announcement.period = announcement.period,
                                 implementation.period = implementation.period,
                                 revocation.period = revocation.period,
                                 in.force.today = in.force.today,
                                 intervention.type = intervention.type,
                                 keep.type = keep.type,
                                 mast.chapter = mast.chapter,
                                 keep.mast = keep.mast,
                                 implementation.level = implementation.level,
                                 keep.level = keep.level,
                                 eligible.firms = eligible.firms,
                                 keep.firms = keep.firms,
                                 cpc.sectors = cpc.sectors,
                                 keep.cpc = keep.cpc,
                                 hs.codes = hs.codes,
                                 keep.hs = keep.hs,
                                 intervention.ids = intervention.ids,
                                 keep.intervention = keep.intervention,
                                 lag.adjustment=lag.adjustment)

  master.slicer <- data_slicer[[2]]

  ##### Extracting Parameter Choices from data slicer
  parameter.choices <- data_slicer[[1]]
  rm(data_slicer)

  ######## Define function parameters

  ###### IMPORTERS / EXPORTERS / IMPLEMENTERS

  ### IMPORTERS
  importing.country=gta_un_code_vector(importers, "importing")

  ### EXPORTERS
  exporting.country=gta_un_code_vector(exporters, "exporting")

  ### IMPLEMENTERS
  implementing.country=gta_un_code_vector(implementers, "implementing")

  ###### IMPLEMENTER ROLES
  if (is.null(implementer.role)==T) {
    implementer.role=c("importer", "3rd country")
  } else {
    implementer.role= tolower(implementer.role)

    if("any" %in% implementer.role){
      implementer.role=c("importer","exporter", "3rd country")
    }

    if(sum(as.numeric((implementer.role %in% c("any","importer","exporter", "3rd country"))==F))>0){
      role.error=paste("'",paste(implementer.role[(implementer.role %in% c("any","importer","exporter", "3rd country"))==F], collapse="; "),"'", sep="")
      print(paste("Unkown implementer role(s): ", role.error, ".", sep=""))
    }
  }

  ## data path for coverage file
  if(data.path.coverage=="online"){
    print("Downloading the latest copy of the GTA trade coverage dataset.The file is deleted after loading the data into your environment.")
    download.file("https://www.dropbox.com/s/s99dho5vhhmjizr/trade%20coverage%20base%20file.Rdata?dl=0","GTA trade coverage data.Rdata")
    load("GTA trade coverage data.Rdata")
    unlink("GTA trade coverage data.Rdata")
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice="Downloaded latest copy"))
  } else{
    load(data.path.coverage)
    parameter.choices=rbind(parameter.choices, data.frame(parameter="Data source:", choice=paste("Local copy from '",data.path,"'.", sep="")))
  }

  # COMMENT: Will we use master.green at any point? then gta.evaluation should be reincluded as parameter to define master set to use.
  master <- master.red
  rm(master.green, master.red)


  ###### Take in data from data slicer and define interventions to keep

  # Column split affected product column
  master.slicer <- cSplit(master.slicer, which(colnames(master.slicer)=="affected.product"), direction="long", sep=",")
  # Take unique column combination of intervention.id, implementer, affected.product
  master.slicer <- unique(master.slicer[,c("intervention.id","implementing.jurisdiction","affected.product")])

  # Merge with master by intervention.id, implementer, affected_product
  setnames(master.slicer, "affected.product", "affected_products")
  setnames(master.slicer, "intervention.id", "intervention_id")
  setnames(master.slicer, "implementing.jurisdiction","implementer")

  master <- merge(master, master.slicer, by=c("intervention_id","implementer","affected_products"))

  # TEST
  length(unique(master.slicer$intervention_id))-length(unique(master$intervention_id))

  rm(master.slicer)

  ###### HS CODES in sample

  products <- unique(master$affected_product)


  ###################################### JF HAS TO CHECK MORE FROM HERE


  #### This belongs to setting imports/exporters. MAybe this is completly useless....
  # Prepare necessary lists for country blocks
  # block.names <- c("G7","G20","EU", "LDC countries", "African Union members")
  # block.numbers <- c(10007,10020,10028,10053,10055)
  # block.abbr <- c("g7", "g20", "eu28", "ldc", "au")
  # block.list <- data.frame(block.abbr, block.names, block.numbers)


  # Check whether sets available and filter for products --------------------

  # If necessary dataframes are not loaded:
  # - Check whether working directory points to GTA cloud to retrieve files
  # - if yes: load sets and directly filter for products if specified
  # - if no: display error message, saying dataframes cannot be found

  ## JF: I would also require reloading the data frames; if someone adjusts the product choices between queries, things blow up otherwise.
  ## PB: data frames are loaded at the beginning to subset master

  # if (endsWith(getwd(), "/GTA cloud")==T) {
  #   print("Loading GTA & trade data files")
  #   load("data/trade coverage/trade coverage base file.Rdata")


  ######## TRADE DATA

  # We have to recalculate the trade data tables if either of those choices is made:
  if(is.null(products)==F|is.null(importers)==F|is.null(exporters)==F){
    print("Loading final.shares from data folder")
    load("data/support tables/final goods trade shares.Rdata")
    setnames(final.shares, old="Reporter.un", "i.un")
    setnames(final.shares, old="Partner.un", "a.un")
    setnames(final.shares, old="Tariff.line", "affected_products")

    if (is.null(products)==F) {
      # COMMENT: With data slicer only 6 digit codes will be delivered
      print("Filtering for products")
      final.shares=subset(final.shares, affected_products %in% products)
    }


    ## correcting for exporters, if stated
    if(is.null(exporters)==F){
      if(is.numeric(exporters)==T){
        final.shares=subset(final.shares, a.un %in% exporters)
      } else {
        print("Exporters need to be specified as UN codes.")
      }
    }

    ## correcting for importers, if stated
    if(is.null(importers)==F){
      if(is.numeric(importers)==T){
        final.shares=subset(final.shares, i.un %in% importers)
      } else {
        print("Importers need to be specified as UN codes.")
      }
    }

    ### replacing import data frames
    print("Create imports and imports.bilateral dataframes")

    imports=aggregate(Value ~ i.un, final.shares, sum)
    imports.bilateral=aggregate(Value ~ i.un + a.un, final.shares, sum)

  } else {
    stop("Necessary dataframes cannot be found. Is 'GTA cloud' your working directory?")
  }


#################### JF IS HERE
  ### check validity of all inputs BEFORE everything else (and subsequently remove those checks further down);
  ### add conversion of country names to UN codes BEFORE anything else
  ### add a function that calculates group trade and have it run for all chosen groups, if any.

  # Defining Country groups -------------------------------------------------

  ## preparation: a correspondence between country/group names and UN codes
  country.correspondence=gtalibrary::country.correspondence
  country.names=gtalibrary::country.names

  # Define country groups:
  country.groups <- subset(country.correspondence, (name %in% country.names$name)==F)
  country.groups <- aggregate(un_code ~ name, country.groups, toString)

  # Check if country groups defined in parameter
  cty.groups.defined <- country.groups[tolower(country.groups$name) %in% tolower(c(importers, exporters, implementers)),]

  # defining country groups
  world=unique(c(imports.bilateral$a.un,imports.bilateral$i.un, master.red$a.un, master.red$i.un, 0))
  eu28=c(251, 276, 381, 826, 40, 56, 100, 191, 196, 203, 208, 233, 246, 300, 348, 372, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752)
  g7=c(251, 276, 381, 826, 124, 392, 840)
  g20=c(32, 36, 76, 124, 156, 251, 276, 699, 360, 381, 392, 484, 410, 643, 682, 710, 792, 826, 840)
  ldc=c(4, 24, 50, 204, 64, 854, 108, 116, 140, 140, 148, 174, 262, 180, 226, 232, 231, 270, 324, 624, 332, 296, 418, 418, 426, 430, 450, 454, 466, 478, 508, 104, 524, 562, 729, 729, 646, 882, 678, 686, 694, 90, 90, 706, 834, 834, 626, 768, 798, 800, 548, 887, 894)
  au=c(12, 24, 204, 72, 854, 108, 120, 132, 140, 148, 174, 180, 262, 818, 226, 232, 231, 266, 270, 288, 324, 624, 384, 404, 426, 430, 434, 450, 454, 466, 478, 480, 504, 508, 516, 562, 566, 178, 646, 732, 678, 686, 690, 694, 706, 710, 729, 748, 834, 768, 788, 800, 894, 716)


  # Create Bilateral Trade Values -------------------------------------------

  ## adding world imports
  imports$a.un=0
  exports=aggregate(Value ~ a.un, imports.bilateral, sum)
  exports$i.un=0

  # Creat dataframe with group numbers and names
  group.un=data.frame(g.id=c("g7", "g20", "eu28", "ldc", "au"), g.un=c(10007, 10020, 10028, 10053, 10055))

  ## Adding G7, g20, LDC, EU28 as observations incl. WORLD combination
  for(group in c("g7", "g20", "eu28", "ldc", "au")){
    eval(parse(text=paste(group,".exports=aggregate(Value ~ i.un, subset(imports.bilateral, a.un %in% ",group,"), sum)  ", sep=""))) #P: Aggregate export values for countries that are part of group
    eval(parse(text=paste(group,".exports$a.un=group.un$g.un[group.un$g.id==group]", sep="")))  #P: Add group number in a.un column
    eval(parse(text=paste(group,".imports=aggregate(Value ~ a.un, subset(imports.bilateral, i.un %in% ",group,"), sum)  ", sep="")))  #P: Aggregate Import values for countries that are part of group
    eval(parse(text=paste(group,".imports$i.un=group.un$g.un[group.un$g.id==group]", sep="")))   #P: Add group number in i.un column
    eval(parse(text=paste(group,".exports.world=data.frame(i.un=0, Value=sum(subset(imports.bilateral, a.un %in% ",group," & i.un <1000)$Value))  ", sep="")))  #P: Create dataframe with i.un == 0 and Value == sum of exports to all countries included in group and exporting to number lower than 1000
    eval(parse(text=paste(group,".exports.world$a.un=group.un$g.un[group.un$g.id==group]", sep="")))  #P: Add group number to exports.world
    eval(parse(text=paste(group,".imports.world=data.frame(a.un=0, Value=sum(subset(imports.bilateral, i.un %in% ",group," & a.un <1000)$Value))  ", sep=""))) #P: Create dataframe with a.un == 0 and Value == sum of imports of all countries included in group and importing from a number lower than 1000
    eval(parse(text=paste(group,".imports.world$i.un=group.un$g.un[group.un$g.id==group]", sep="")))  #P: Add group number to dataframe
    eval(parse(text=paste(group,".intra=data.frame(i.un=group.un$g.un[group.un$g.id==group], a.un=group.un$g.un[group.un$g.id==group], Value=sum(subset(imports.bilateral, a.un %in% ",group," & i.un %in% ",group,")$Value))  ", sep="")))  #P: Create dataframe where i.un and a.un == group number and Value == sum of imports and exports to or from countries included in the group
    eval(parse(text=paste("imports.bilateral=rbind(imports.bilateral, ",group,".intra, ",group,".imports, ",group,".imports.world, ",group,".exports, ",group,".exports.world)", sep="")))  #P: rbind the created dataframes with the existing ones
    eval(parse(text=paste("rm(",group,".intra, ",group,".imports, ",group,".imports.world, ",group,".exports, ",group,".exports.world)", sep=""))) #P: Remove unnecessary dfs
  }

  ## intra group trade
  group.combos=subset(expand.grid(group.un$g.un,group.un$g.un), Var1!=Var2)
  colnames(group.combos)=c("a.un", "i.un")
  group.combos$Value=NA

  for(i in 1:nrow(group.combos)){
    group.1=group.un$g.id[group.un$g.un==group.combos$a.un[i]]
    group.2=group.un$g.id[group.un$g.un==group.combos$i.un[i]]
    eval(parse(text=paste("group.combos$Value[i]=sum(subset(imports.bilateral, a.un %in% ",group.1," & i.un %in% ",group.2,")$Value)", sep=""))) #P: Fill in values of intra group trade by iterating through group.combos
  }

  imports.bilateral=unique(rbind(group.combos,imports.bilateral, imports, exports))


  # REMOVE AFTER TESTING: Remove 99/100 of all rows to increase computing speed for writing function
  if (testmode==T){
    keep.rows = seq(1,length(master$i.un),100)
    master = master[keep.rows,]
  }

  # Filtering HS Categories and CPC Sectors --------------------------------------------

  # comment-done: move to the top

  # if HS list exists, remove affected products not mentioned in HS list
  if (is.null(products)==F) {
    master$affected_products <- sprintf("%06i",master$affected_products)
    master$tariff_line_code_2 <- substr(master$affected_products, 1,2)
    master = subset(master, affected_products %in% products | tariff_line_code_4 %in% products | tariff_line_code_2 %in% products)
  }

  # if CPC list exists, remove affected sectors not mentioned in CPC list
  if (is.null(sectors)==F) {
    master$sector_code <- as.factor(sprintf("%03i",master$sector_code))
    master$sector.2 <- as.factor(substr(master$sector_code, 1,2))
    sectors <- as.factor(sprintf("%02i", sectors))
    master = subset(master, sector_code %in% sectors | sector.2 %in% sectors)
  }

  # comment: check more thoroughly

  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values existing for chosen parameters (last filtered for HS and CPC codes)")}


  # Removing LCRs -------------------------------------------------------------

  # Removing LCRs that are part of trade finance

  # Local sourcing has measure_type_id 28 and trade finance 54
  # I only remove interventions which are part of a state act that includes a trade finance & LCR only (not budgets etc).
  if (endsWith(getwd(), "/GTA cloud")==T) {
    print("Removing LCRs that are part of trade finance")
    gta_intervention=read.csv("data/database replica/gta_intervention.csv", sep=",")
    master=subset(master, ! intervention_id %in% subset(gta_intervention, measure_type_id==28 & measure_id %in% subset(gta_intervention, measure_type_id==54 & measure_id %in% subset(as.data.frame.table(table(gta_intervention$measure_id)), Freq==2)$Var1)$measure_id)$id)

  } else {
    stop("gta_intervention.csv cannot be found")
  }


  # Calculations and creating master.max ------------------------------------------------------------

  #P: Computing values for each year depending on share of coverage --> calculated in 0 data prep.
  for(yr in eval(parse(text=paste0(start.year,":",end.year)))){
    eval(parse(text=paste("master$value.",yr,"=master$value.",yr,"*master$Value", sep="")))
  }

  ## Adding nicer MAST titles
  master$mast.title=paste(master$mast.chapter, master$chapter.label, sep=" - ")


  ## create a master.max to begin with. no need to take account of all 5mn rows.
  ## master max
  print(paste0(start.year," | Create a master.max"))
  eval(parse(text=paste0("master.max=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + t.un + implementer + mast.title + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(from+1,":",end.year)))){
    print(paste0(loop.yr, " | Create a master max"))
    eval(parse(text=paste("master.max=merge(master.max, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + t.un + implementer +  mast.title + affected_products, master, max),
                          by=c('importer','i.un','exporter','a.un', 't.un', 'implementer', 'mast.title', 'affected_products'), all.x=T)", sep="")))

  }

  master=master.max
  rm(master.max)


  # Calculating world and country groups values and filter for exporters/importers/implementers -----------------------------

  # Calculate Country groups
  master.backup <- master
  master.groups <- master.backup

  # block.list$block.abbr <- as.character(block.list$block.abbr)

  # Find out which groups were chosen
  groups.i <- subset(block.list, block.list[,1] %in% importers |
                       block.list[,2] %in% importers |
                       block.list[,3] %in% importers)
  groups.e <- subset(block.list, block.list[,1] %in% exporters |
                       block.list[,2] %in% exporters |
                       block.list[,3] %in% exporters)
  groups.t <- subset(block.list, block.list[,1] %in% implementers |
                       block.list[,2] %in% implementers |
                       block.list[,3] %in% implementers)
  row.names(groups.i) <- NULL
  row.names(groups.e) <- NULL
  row.names(groups.t) <- NULL


  # Filter country groups in importers (add rows per group)
  if (nrow(groups.i) != 0) {
    print("Add groups as importers")
    for(i in 1:nrow(groups.i)){
      print(paste0("Adding ",groups.i[i,2]))
      master.temp = subset(master.backup, i.un %in% eval(parse(text=paste0(groups.i[i,1]))))
      master.temp$importer = groups.i[i,2]
      master.temp$i.un = groups.i[i,3]
      master.groups = rbind(master.groups, master.temp)
      rm(master.temp)
    }
  }

  # Filter country groups in exporters (add rows per group)
  if (nrow(groups.e) != 0) {
    print("Add groups as exporters")
    for(e in 1:nrow(groups.e)){
      print(paste0("Adding ",groups.e[e,2]))
      master.temp = subset(master.backup, a.un %in% eval(parse(text=paste0(groups.e[e,1]))))
      master.temp$exporter = groups.e[e,2]
      master.temp$a.un = groups.e[e,3]
      master.groups = rbind(master.groups, master.temp)
      rm(master.temp)
    }
  }

  # Filter country groups in implementers (add rows per group)
  if (nrow(groups.t) != 0) {
    print("Add groups as implementers")
    for(t in 1:nrow(groups.t)){
      print(paste0("Adding ",groups.t[t,2]))
      master.temp = subset(master.backup, t.un %in% eval(parse(text=paste0(groups.t[t,1]))))
      master.temp$implementer = groups.t[t,2]
      master.temp$t.un = groups.t[t,3]
      master.groups = rbind(master.groups, master.temp)
      rm(master.temp)
    }
  }


  # Add country groups for in between group trades
  if (nrow(groups.i) != 0 & nrow(groups.e) != 0) {
    print("Add trade values between groups importers and exporters")

    for(i in 1:nrow(groups.i)){
      for(e in 1:nrow(groups.e)){
        print(paste0("Adding ",groups.i[i,2], " with ", groups.e[e,2]))
        master.temp = subset(master.backup, i.un %in% eval(parse(text=paste0(groups.i[i,1]))) & a.un %in% eval(parse(text=paste0(groups.e[e,1]))))
        if (nrow(master.temp) != 0){
          master.temp$importer = groups.i[i,2]
          master.temp$i.un = groups.i[i,3]
          master.temp$exporter = groups.e[e,2]
          master.temp$a.un = groups.e[e,3]
          master.groups = rbind(master.groups, master.temp)
        }
        rm(master.temp)
      }
    }
  }

  if (nrow(groups.i) != 0 & nrow(groups.t) != 0) {
    print("Add trade values between groups importers and implementers")

    for(i in 1:nrow(groups.i)){
      for(t in 1:nrow(groups.t)){
        print(paste0("Adding ",groups.i[i,2], " with ", groups.t[t,2]))
        master.temp = subset(master.backup, i.un %in% eval(parse(text=paste0(groups.i[i,1]))) & t.un %in% eval(parse(text=paste0(groups.t[t,1]))))
        if (nrow(master.temp) != 0){
          master.temp$importer = groups.i[i,2]
          master.temp$i.un = groups.i[i,3]
          master.temp$implementer = groups.t[t,2]
          master.temp$t.un = groups.t[t,3]
          master.groups = rbind(master.groups, master.temp)
        }
        rm(master.temp)
      }
    }
  }

  if (nrow(groups.e) != 0 & nrow(groups.t) != 0) {
    print("Add trade values between groups exporters and implementers")

    for(e in 1:nrow(groups.e)){
      for(t in 1:nrow(groups.t)){
        print(paste0("Adding ",groups.e[e,2], " with ", groups.t[t,2]))
        master.temp = subset(master.backup, a.un %in% eval(parse(text=paste0(groups.e[e,1]))) & t.un %in% eval(parse(text=paste0(groups.t[t,1]))))
        if (nrow(master.temp) != 0){
          master.temp$exporter = groups.e[e,2]
          master.temp$a.un = groups.e[e,3]
          master.temp$implementer = groups.t[t,2]
          master.temp$t.un = groups.t[t,3]
          master.groups = rbind(master.groups, master.temp)
        }
        rm(master.temp)
      }
    }
  }

  if (nrow(groups.e) != 0 & nrow(groups.t) != 0 & nrow(groups.t) != 0) {
    print("Add trade values between groups importers, exporters and implementers")

    for(i in 1:nrow(groups.i)){
      for(e in 1:nrow(groups.e)){
        for(t in 1:nrow(groups.t)){
          print(paste0("Adding ",groups.i[i,2], " with ", groups.e[e,2], " and ", groups.t[t,2]))
          master.temp = subset(master.backup, i.un %in% eval(parse(text=paste0(groups.i[i,1]))) & a.un %in% eval(parse(text=paste0(groups.e[e,1]))) & t.un %in% eval(parse(text=paste0(groups.t[t,1]))))
          if (nrow(master.temp) != 0){

            master.temp$importer = groups.i[i,2]
            master.temp$i.un = groups.i[i,3]
            master.temp$exporter = groups.e[e,2]
            master.temp$a.un = groups.e[e,3]
            master.temp$implementer = groups.t[t,2]
            master.temp$t.un = groups.t[t,3]
            master.groups = rbind(master.groups, master.temp)
          }
          rm(master.temp)
        }
      }
    }
  }


  # Creating world and country groups values and add them to the base master
  # Save dataframe for later generation of world and group data
  print("Add World as importer and exporter and implementer")

  master.world.imp <- master.groups
  master.world.imp$i.un = 0
  master.world.imp$importer = "World"
  master.world.exp <- master.groups
  master.world.exp$a.un = 0
  master.world.exp$exporter = "World"
  master.world.intra <- master.backup
  master.world.intra$a.un = 0
  master.world.intra$exporter = "World"
  master.world.intra$i.un = 0
  master.world.intra$importer = "World"

  master.world <- rbind(master.world.imp, master.world.exp, master.world.intra)
  master <- rbind(master.world, master.groups)
  rm(master.world.imp, master.world.exp, master.world.intra)

  if(is.null(importers)==F) {
    print("Filtering for chosen importers: ")
    print(importers)
    master <- subset(master, i.un %in% importers | tolower(importer) %in% tolower(importers) | importer == "World")
  }

  if(is.null(exporters)==F) {
    print("Filtering for chosen exporters: ")
    print(exporters)
    master <- subset(master, a.un %in% exporters | tolower(exporter) %in% tolower(exporters) | exporter == "World")
  }

  if(is.null(implementers)==F){
    print("Filtering for chosen implementers: ")
    print(implementers)
    master <- subset(master, t.un %in% implementers | tolower(implementer) %in% tolower(implementers))

  } else {
    print("No implementer specified. Implementer == Any")
    master$t.un <- 0
    master$implementer <- "World"
  }


  # Setting the implementer type / create master without implementer countries --------------------------------------------

  # Setting the implementer
  master$implementer.type="importer"
  master$implementer.type[master$a.un==master$t.un]="exporter" #P: if affected jurisdiction == implementing jurisdiction: exporter
  master$implementer.type[master$a.un!=master$t.un &
                            master$i.un!=master$t.un]="3rd country" #P: If affected jurisdiction != implementer & i.un != implementer: then 3rd country must be implementer

  # remove implementer column
  master <- subset(master, select=-c(implementer,t.un))


  # Adding additional implementer types -------------------------------------

  ## Adding ANY implementer type
  print(paste0(start.year," | Adding ANY implementer type"))
  eval(parse(text=paste0("imp.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + mast.title + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
    print(paste0(loop.yr, " | Adding ANY implementer type"))
    eval(parse(text=paste("imp.any=merge(imp.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + mast.title + affected_products, master, max),
                          by=c('importer','i.un','exporter','a.un','mast.title', 'affected_products'), all.x=T)", sep="")))

  } #P: imp.any will include all aggregated max-values from value.2009 to value.2017, then add implementer.type = "any" to it

  imp.any$implementer.type="any"
  master=rbind(imp.any, master)
  rm(imp.any)


  ## Adding implementer is importer or exporter combination
  combo=c("importer", "exporter")

  if(any(unique(master$implementer.type) %in% combo) == T){
    print(paste0(start.year," | Adding implementer is importer or exporter combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
      print(paste0(loop.yr, " | Adding implementer is importer or exporter combination"))
      eval(parse(text=paste("imp.any=merge(imp.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max),
                            by=c('importer','i.un','exporter','a.un','mast.title', 'affected_products'), all.x=T)", sep="")))
    }

    imp.any$implementer.type="importer or exporter"
    master=rbind(imp.any, master)
    rm(imp.any)
    }

  ## Adding implementer is importer or 3rd country combination
  combo=c("importer", "3rd country")

  if(any(unique(master$implementer.type) %in% combo) == T){
    print(paste0(start.year," | Adding implementer is importer or 3rd country combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
      print(paste0(loop.yr, " | Adding implementer is importer or 3rd country combination"))
      eval(parse(text=paste("imp.any=merge(imp.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max),
                            by=c('importer','i.un','exporter','a.un','mast.title', 'affected_products'), all.x=T)", sep="")))
    }
    imp.any$implementer.type="importer or 3rd country"
    master=rbind(imp.any, master)
    rm(imp.any)
    }


  ## Adding implementer is exporter or 3rd  combination
  combo=c("3rd country", "exporter")

  if(any(unique(master$implementer.type) %in% combo) == T){
    print(paste0(start.year," | Adding implementer is exporter or 3rd  combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
      print(paste0(loop.yr, " | Adding implementer is exporter or 3rd  combination"))
      eval(parse(text=paste("imp.any=merge(imp.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max),
                            by=c('importer','i.un','exporter','a.un','mast.title', 'affected_products'), all.x=T)", sep="")))
    }

    imp.any$implementer.type="exporter or 3rd country"
    master=rbind(imp.any, master)
    rm(imp.any)
    }


  ## adding MAST chapter total
  print(paste0(start.year," | adding MAST chapter total"))
  eval(parse(text=paste0("mast.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
    print(paste0(loop.yr, " | adding MAST chapter total"))
    eval(parse(text=paste("mast.any=merge(mast.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, master, max),
                          by=c('importer','i.un','exporter','a.un','implementer.type','affected_products'), all.x=T)", sep="")))
  }

  mast.any$mast.title="All instruments"

  master=rbind(mast.any, master[, c(colnames(mast.any))])
  rm(mast.any)



  # if instruments are chosen, calculate the total of the chosen instruments
  if(is.null(instruments)==F){
    if(tolower(instruments[length(instruments)])!= "include") {

    ## adding MAST chapter total of chosen instruments
    print(paste0(start.year," | adding MAST chapter total of chosen instruments"))
    eval(parse(text=paste0("mast.any=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, subset(master, mast.title != 'All instruments'), max)")))
    for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
      print(paste0(loop.yr, " | adding MAST chapter total of chosen instruments"))
      eval(parse(text=paste("mast.any=merge(mast.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, subset(master, mast.title != 'All instruments'), max),
                            by=c('importer','i.un','exporter','a.un','implementer.type','affected_products'), all.x=T)", sep="")))
    }
      mast.any$mast.title="Chosen instruments"

      master=rbind(mast.any, master[, c(colnames(mast.any))])
      rm(mast.any)
    }
  }



  # If instruments == null, keep only "All Instruments"
  if(is.null(instruments)==T) {
    master <- subset(master, mast.title == "All instruments")
  }



  ## aggregating to the national level
  print(paste0(start.year," | Aggregating to the national level"))
  eval(parse(text=paste0("mast.agg=aggregate(value.",start.year," ~ importer + i.un + exporter + a.un + implementer.type + mast.title, master, sum)"))) #P: Removing affected products column by summing it up.
  for(loop.yr in eval(parse(text=paste0(start.year+1,":",end.year)))){
    print(paste0(loop.yr, " | Aggregating to the national level"))
    eval(parse(text=paste("mast.agg=merge(mast.agg, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + implementer.type + mast.title, master, sum),
                          by=c('importer','i.un','exporter','a.un','implementer.type', 'mast.title'), all.x=T)", sep="")))

  }

  master=mast.agg
  rm(mast.agg)




  # Filter for implementer role ---------------------------------------------

  if (is.null(implementer.role)==F) {
    print("Filter for implementer role")

    if (tolower(implementer.role[1]) == "all combinations") {
      master <- master

    } else if (tolower(implementer.role[1])=="keep"){
      master <- subset(master, implementer.type %in% implementer.role)

    } else if (tolower(implementer.role[1])=="remove"){
      master <- subset(master, ! implementer.type %in% implementer.role)

    } else {
      master <- subset(master, implementer.type %in% implementer.role)
    }
  }



  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values left after filtering for implementer type")}


  # Calculate coverage shares ---------------------------------------------------------

  ## add bilateral trade, divide, done.
  master=merge(master, imports.bilateral, by=c("a.un", "i.un"), all.x=T)
  master$Value[master$i.un==0 & master$a.un==0] <- sum(imports$Value)
  master$Value[is.na(master$Value)]=1
  master$Value[master$Value==0]=1

  #Calculate coverage shares by dividing yearly numbers with overall trade value
  for(loop.yr in eval(parse(text=paste0(start.year,":",end.year)))){
    print(paste0(loop.yr, " | Calculating Shares"))
    eval(parse(text=paste("master$value.",loop.yr,"=master$value.",loop.yr,"/master$Value", sep="")))
    eval(parse(text=paste("master$value.",loop.yr,"[master$value.",loop.yr,">=1]=1", sep="")))
  }

  summary(master)
  print(master)


  # Output ------------------------------------------------------------------

  master$Value=NULL
  master$a.un=NULL
  master$i.un=NULL


  #  comment: output to rdata

  if(rdata == T){
    print("Save Rdata file")
    path=paste(output.path,"trade coverage estimates - ",evaluation," - ",Sys.Date(),".Rdata", sep="")
    save(master, file=path)
  }



  coverage.total.xlsx=master[,colnames(master)]

  number_of_years = end.year-start.year
  column_names  = c("Importing country", "Exporting country", "Implementing country", "Policy instrument",
                    "Percentage 2009", "Percentage 2010", "Percentage 2011", "Percentage 2012", "Percentage 2013", "Percentage 2014", "Percentage 2015", "Percentage 2016", "Percentage 2017", "Percentage 2018", "Percentage 2019", "Percentage 2020", "Percentage 2021")
  column_names = column_names[1:ncol(coverage.total.xlsx)]

  colnames(coverage.total.xlsx)=column_names

  parameters <- data.frame(Parameters = c("Evaluation",
                                          "Flow",
                                          "Importers",
                                          "Exporters",
                                          "Implementers",
                                          "Implementer.role",
                                          "Products",
                                          "Sectors",
                                          "Instruments",
                                          "Inception.range",
                                          "In.force",
                                          "Implementation.level",
                                          "Eligible.firms"), Value = 0)

  for (i in 1:nrow(parameters)) {
    if (is.null(eval(parse(text=paste0(tolower(parameters[i,1])))))==T) {
      parameters[i,2] <- "Not specified"
    } else {
      parameters[i,2] <- paste(eval(parse(text=paste0(tolower(parameters[i,1])))), collapse=", ")
    }
  }

  if(is.null(implementer.role)==T) {
    if(flow=="imports"){
      parameters$Value[parameters$Parameters=="Implementers"] <- "importer"
    } else {
      parameters$Value[parameters$Parameters=="Implementers"] <- "importer or 3rd country"
    }
  }

  print("Save excel file")

  write.xlsx(coverage.total.xlsx, file=paste0(getwd(),output.path,"Trade coverage estimate - ",evaluation," - ", today,".xlsx"),
             sheetName="Results", append=FALSE, row.names=FALSE)
  write.xlsx(parameters, file=paste0(getwd(),output.path,"Trade coverage estimate - ",evaluation," - ", today,".xlsx"),
             sheetName="Parameters", append=TRUE, row.names=FALSE)

  # End of package
  return(master)

  }
