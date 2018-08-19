

# Function infos and parameters -------------------------------------------

gtatradecoverage <- function(evaluation = NULL,
                           flow = NULL,
                           importers = NULL,
                           exporters = NULL,
                           implementers = NULL,
                           implementer.role = NULL,
                           products = NULL,
                           sectors = NULL,
                           instruments = NULL,
                           inception.range = c("2009-01-01", NA),
                           in.force = NULL,
                           implementation.level = NULL,
                           eligible.firms = NULL,
                           rdata = FALSE,
                           output.path = NULL,
                           testmode = FALSE) {

                          evaluation = "harmful"
                          flow = "imports"
                          importers = NULL
                          exporters = c("china","Australia")
                          implementers = NULL
                          implementer.role = NULL
                          products = NULL
                          sectors = NULL
                          instruments = NULL
                          inception.range = c("2009-01-01", NA)
                          in.force = NULL
                          implementation.level = NULL
                          eligible.firms = NULL
                          rdata = FALSE
                          output.path = NULL
                          testmode = TRUE


  # Initialising Function ---------------------------------------------------

  # load libraries
  require("splitstackshape")
  require("foreign")
  require("openxlsx")
  require("scales")

  # Store today's date
  today = Sys.Date()
  # years to calculate
  from = 2009
  to = 2018

  # Prepare necessary lists for country blocks
  block.names <- c("G7","G20","EU", "LDC countries", "African Union members")
  block.numbers <- c(10007,10020,10028,10053,10055)
  block.abbr <- c("g7", "g20", "eu28", "ldc", "au")
  block.list <- data.frame(block.abbr, block.names, block.numbers)


  # Check whether sets available and filter for products --------------------

  # If necessary dataframes are not loaded:
  # - Check whether working directory points to GTA cloud to retrieve files
  # - if yes: load sets and directly filter for products if specified
  # - if no: display error message, saying dataframes cannot be found

  if ((exists("imports") & exists("imports.bilateral")) == F) {

    if (endsWith(getwd(), "/GTA cloud")==T) {
      print("Loading trade coverage base file")
      load("data/trade coverage/trade coverage base file.Rdata")

      if (is.numeric(products)==F & is.null(products)==F) {
        stop("The products list does contain values other than numerics")
      }

      if (is.null(products) == F) {
        print("Loading final.shares from data folder")
        load(paste0(getwd(), "/data/support tables/final goods trade shares.Rdata"))
        setnames(final.shares, old="Reporter.un", "i.un")
        setnames(final.shares, old="Partner.un", "a.un")
        setnames(final.shares, old="Tariff.line", "affected_products")

        # Add tariff_line_code_4, because otherwise trade values and interventions will be filtered differently
        print("Filtering for products")
        final.shares$affected_products.4 <-  substr(final.shares$affected_products, 1,4)
        final.shares$affected_products.2 <- substr(final.shares$affected_products, 1,2)

        ### check those totals!!
        print("Create imports and imports.bilateral dataframes")
        imports=aggregate(Value ~ i.un, subset(final.shares, affected_products %in% products | affected_products.4 %in% products | affected_products.2 %in% products), sum)
        imports.bilateral=aggregate(Value ~ i.un + a.un, subset(final.shares, affected_products %in% products | affected_products.4 %in% products | affected_products.2 %in% products), sum)

      }
    } else {
      stop("Necessary dataframes cannot be found and working directory does not point to GTA cloud folder for retrieving")
    }

  } else {
    if (is.numeric(products)==F & is.null(products)==F) {
      stop("The products list does contain values other than numerics")
    }

    if (is.null(products) == F) {
      # Add tariff_line_code_4, because otherwise trade values and interventions will be filtered differently
      print("Filtering for products")
      final.shares$affected_products.4 = substr(final.shares$affected_products, 1,4)

      ### check those totals!!
      print("Create imports and imports.bilateral dataframes")
      imports=aggregate(Value ~ i.un, subset(final.shares, affected_products %in% products | affected_products.4 %in% products |affected_products.2 %in% products), sum)
      imports.bilateral=aggregate(Value ~ i.un + a.un + affected_products.4 + affected_products, subset(final.shares, affected_products %in% products | affected_products.4 %in% products | affected_products.2 %in% products), sum)
    } else {
      print("No changes made to the dataframes")
    }
  }


  # Defining Country groups -------------------------------------------------

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



  # Check whether master-set is availabe -------------------------------------

  if (exists("master.green") & exists("master.red") == F) {
    stop("Master sets cannot be found")
  }

  # Removing unused master set ------------------------------------------------

  # delete unused master depending on

  if (is.null(evaluation)==T) {
    stop("No evaluation type defined")
  }
  if (evaluation == "harmful") {
    print("Only harmful interventions chosen")
    master <- master.red
  }
  if (evaluation == "liberalising") {
    print("Only liberalising interventions chosen")
    master <- master.green
  }

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


  # Currently in force and inside duration specified--------------------------------------------
  ## Filtering for currently in force
  if (is.null(in.force)==F) {

    if (in.force == TRUE) {
      print("Filtering out interventions not in force anymore")
      master=subset(master, is.na(master$removal_date)==T | removal_date >= today)
    }

    if (in.force == FALSE) {
      print("Filtering out interventions which are still in force")
      master=subset(master, is.na(master$removal_date)==F & removal_date <= today)
    }

  } else {
    print("Interventions in force as well as not in force included")
  }


  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values existing for chosen parameters (last filtered for in.force)")}

  ## Filtering for start and end date
  if (inception.range[1] != "2009-01-01") {
    print(paste0("Filtering interventions before ",inception.range[1]))
    master <- subset(master, inception_date >= inception.range[1])
  } else {
    print(paste0("Filtering interventions before ",inception.range[1]))
    master <- subset(master, inception_date >= inception.range[1])
  }

  if (is.na(inception.range[2])==F) {
    print(paste0("Filtering interventions after ",inception.range[2]))
    master <- subset(master, inception_date <= inception.range[2])
  }

  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values existing for chosen parameters (last filtered for inception date)")}


  # Filtering MAST Chapters -------------------------------------------------

  if (is.null(instruments)==F) {
    if (tolower(instruments[1])=="remove"){
      print(paste0("Removing interventions with MAST Chapter ", instruments))
      master <- subset(master, ! tolower(mast.chapter) %in% tolower(instruments))
    }
    if (tolower(instruments[1])=="keep" | tolower(instruments[1])!="remove"){
      print(paste0("Keeping instruments with MAST Chapter ", instruments))
      master <- subset(master, tolower(mast.chapter) %in% tolower(instruments))
    }
  }

  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values existing for chosen parameters (last filtered for MAST Chapters)")}


  # Setting implementation level --------------------------------------------

  ## Setting the implementation level, if of interest (1=supra, 2=nation, 3=subnat, 4=SEZ, 5=IFI, 6=NFI)

  if (is.null(implementation.level)==F) {

    if (exists("gta_intervention") == F) {

      if (endsWith(getwd(), "/GTA cloud")==T) {
        print("Load gta_intervention dataframe")
        print("Filtering for chosen intervention levels")
        gta_intervention=read.csv("data/database replica/gta_intervention.csv", sep=",")

      } else {
        stop("gta_interventions dataframe cannot be found and working directory does not point to GTA cloud folder for retrieving")
      }
    }

    if (tolower(implementation.level[1])=="remove") {
      print(paste0("Removing these following implementation levels: ", implementation.level))
      master=subset(master, intervention_id %in% subset(gta_intervention, ! implementation_level_id %in% implementation.level)$id)
    }

    if (tolower(implementation.level[1])=="keep" | tolower(implementation.level[1]!="remove")) {
      print(paste0("Keeping these implementation levels:", implementation.level))
      master=subset(master, intervention_id %in% subset(gta_intervention, implementation_level_id %in% implementation.level)$id)
    }
  }

  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values left after filtering for implementation levels")}

  # Setting eligible firms --------------------------------------------------

  ## Setting the eligible firms box, if of interest (1= all, 2=SME, 3=firm-specific, 4= state-controlled, 5=state-trading)

  if (is.null(eligible.firms)==F) {

    if (exists("gta_intervention") == F) {

      if (endsWith(getwd(), "/GTA cloud")==T) {
        print("Load gta_intervention dataframe")
        print("Filtering for chosen eligible firms categories")
        gta_intervention=read.csv("data/database replica/gta_intervention.csv", sep=",")

      } else {
        stop("gta_interventions dataframe cannot be found and working directory does not point to GTA cloud folder for retrieving")
      }
    }

    if (tolower(eligible.firms[1])=="remove") {
      print(paste0("Removing these eligible firms categorie: ", eligible.firms))
      master=subset(master, intervention_id %in% subset(gta_intervention, ! eligible_firms_id %in% eligible.firms)$id)
    }

    if (tolower(eligible.firms[1])=="keep" | tolower(eligible.firms[1]!="remove")) {
      print(paste0("Keeping these eligible firms categories:", eligible.firms))
      master=subset(master, intervention_id %in% subset(gta_intervention, eligible_firms_id %in% eligible.firms)$id)
    }
  }

  # Check if master is empty, stop function
  if(nrow(master)==0){stop("No values left after filtering for eligible firms categories")}


  # Calculations and creating master.max ------------------------------------------------------------
  master.backup.max <- master
  master <- master.backup.max
  #P: Computing values for each year depending on share of coverage --> calculated in 0 data prep.
  for(yr in eval(parse(text=paste0(from,":",to)))){
    eval(parse(text=paste("master$value.",yr,"=master$value.",yr,"*master$Value", sep="")))
  }

  ## Adding nicer MAST titles
  master$mast.title=paste(master$mast.chapter, master$chapter.label, sep=" - ")


  ## create a master.max to begin with. no need to take account of all 5mn rows.
  ## master max
  print(paste0(from," | Create a master.max"))
  eval(parse(text=paste0("master.max=aggregate(value.",from," ~ importer + i.un + exporter + a.un + t.un + implementer + mast.title + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
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

  master.temp <- master.groups
  master.groups <- master.temp

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
  print(paste0(from," | Adding ANY implementer type"))
  eval(parse(text=paste0("imp.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + mast.title + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
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
    print(paste0(from," | Adding implementer is importer or exporter combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
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
    print(paste0(from," | Adding implementer is importer or 3rd country combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
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
    print(paste0(from," | Adding implementer is exporter or 3rd  combination"))
    eval(parse(text=paste0("imp.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max)")))
    for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
      print(paste0(loop.yr, " | Adding implementer is exporter or 3rd  combination"))
      eval(parse(text=paste("imp.any=merge(imp.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + mast.title + affected_products, subset(master, implementer.type %in% combo), max),
                            by=c('importer','i.un','exporter','a.un','mast.title', 'affected_products'), all.x=T)", sep="")))
    }

    imp.any$implementer.type="exporter or 3rd country"
    master=rbind(imp.any, master)
    rm(imp.any)
    }

  master.backup.mast <- master
  master <- master.backup.mast


  ## adding MAST chapter total
  print(paste0(from," | adding MAST chapter total"))
  eval(parse(text=paste0("mast.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, master, max)")))
  for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
    print(paste0(loop.yr, " | adding MAST chapter total"))
    eval(parse(text=paste("mast.any=merge(mast.any, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, master, max),
                          by=c('importer','i.un','exporter','a.un','implementer.type','affected_products'), all.x=T)", sep="")))
  }

  mast.any$mast.title="All instruments"

  master=rbind(mast.any, master[, c(colnames(mast.any))])
  rm(mast.any)



  # if instruments are chosen, calculate the total of the chosen instruments
  if(is.null(instruments)==F){
    if(tolower(instruments[1])!= "include") {

    ## adding MAST chapter total of chosen instruments
    print(paste0(from," | adding MAST chapter total of chosen instruments"))
    eval(parse(text=paste0("mast.any=aggregate(value.",from," ~ importer + i.un + exporter + a.un + implementer.type + affected_products, subset(master, mast.title != 'All instruments'), max)")))
    for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
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
  print(paste0(from," | Aggregating to the national level"))
  eval(parse(text=paste0("mast.agg=aggregate(value.",from," ~ importer + i.un + exporter + a.un + implementer.type + mast.title, master, sum)"))) #P: Removing affected products column by summing it up.
  for(loop.yr in eval(parse(text=paste0(from+1,":",to)))){
    print(paste0(loop.yr, " | Aggregating to the national level"))
    eval(parse(text=paste("mast.agg=merge(mast.agg, aggregate(value.",loop.yr," ~ importer + i.un + exporter + a.un + implementer.type + mast.title, master, sum),
                          by=c('importer','i.un','exporter','a.un','implementer.type', 'mast.title'), all.x=T)", sep="")))

  }

  master=mast.agg
  rm(mast.agg)




  # Filter for implementer role ---------------------------------------------

  if (is.null(implementer.role)==F) {
    print("Filter for implementer role")

    if (tolower(implementer.role) == "all combinations") {
      master <- master
    }
    if (tolower(implementer.role[1])=="keep" | tolower(implementer.role[1]!="remove")){
      master <- subset(master, implementer.type %in% implementer.role)
    }
    if (tolower(implementer.role[1])=="remove"){
      master <- subset(master, ! implementer.type %in% implementer.role)
    }
    else {
      master <- subset(master, implementer.type %in% implementer.role)
    }
  }
  if (is.null(implementer.role)==T) {
    if (tolower(flow) == "imports") {
      master <- subset(master, implementer.type == "importer")
    }
    if (tolower(flow) == "exports") {
      master <- subset(master, implementer.type == "importer or 3rd country")
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
  for(loop.yr in eval(parse(text=paste0(from,":",to)))){
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

  number_of_years = to-from
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
  write.xlsx(x=list("Results"=coverage.total.xlsx,"Parameters"=parameters), file=paste0(output.path,"Trade coverage estimate - ",evaluation," - ", today,".xlsx"))


  # End of package
  return(master)

  }
