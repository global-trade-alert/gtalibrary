# Roxygen documentation

#' Retrieves the hs/cpc codes of the firms that have already been uploaded to the database in previous data dumps. To do this, once the function is called it will be necessary to provide a string in the terminal containing a part of the name of the data dump by which you want to filter. For example, spending, if we want to search the USAspending.gov dump.
#'
#' @param master Final master data frame that will be needed to collect the local intervention types from the data dump
#' @param firms.df Final data frame containing the firms that will be added to the interventions in a specific data dump
#'
#' @returns List containing a data frame with the interventions,types of interventions and firms to be uploaded and their corresponding hs codes in first position and another one with the cpc codes in second position
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_firm_related_codes_last_dumps = function(master,
                                             firms.df) {

  library(gtasql)
  library(pool)
  library(DBI)
  library(RMariaDB)

  # DB setup
  gta_sql_kill_connections()

  database <- 'gtamain.readonly'

  gta_sql_pool_open(db.title = database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "gta_")

  # We make sure that there are no rows with an empty value in the firm.name field
  # of the firms.df
  firms.df = firms.df[complete.cases(firms.df$firm.name), ]

  # We escape the single quotation marks that may be present in the firm names
  firms.df$escaped.firm.name = gsub("'", "''", firms.df$firm.name)

  # Link the types of intervention to their corresponding firm
  firms.df = merge(firms.df, master[,c("intervention.id", "intervention.type")], by = "intervention.id", all.x = T)
  colnames(firms.df)[colnames(firms.df) == "intervention.type"] <- "dump.intervention.type"


  # Perform the search in the DB
  codes.last.dumps = gta_sql_get_value(
    paste0("select distinct mfl.firm_name, mfl.firm_name_original, gmt.name as BD_intervention_type,
    gatl.tariff_line_code as hs_code,gas.sector_code as cpc_code, gdl.dump_name
    from gta_intervention_dump gid
    join gta_intervention gi on gi.id = gid.intervention_id
    join gta_measure gm on gm.id = gi.measure_id
    join gta_import_firm_log gifl on gifl.intervention_id = gid.intervention_id
    join mtz_firm_log mfl on mfl.firm_id = gifl.firm_id
    join gta_dump_log gdl on gdl.dump_id = gid.dump_id
    join gta_measure_type gmt on gmt.id = gi.measure_type_id
    left join gta_affected_tariff_line gatl on gatl.intervention_id = gi.id
    left join gta_affected_sector gas on gas.intervention_id = gi.id
    where gm.status_id = 4
    and gdl.dump_name like '%", readline("Type the name of the data dump you want to filter by: "), "%'
    and (gatl.tariff_line_code is not null or gas.sector_code is not null)
    and mfl.firm_name in (", paste(paste0("'", unique(firms.df$escaped.firm.name), "'"), collapse = ','), ");"
    ))

  if (nrow(codes.last.dumps) > 0) {
  # We join the data frames to obtain the interventions that will contain hs/cpc codes. In addition, we will
  # see if the match occurs only by firm or by firm and type of intervention
  int.codes = merge(codes.last.dumps[, c("firm.name", "BD.intervention.type", "hs.code", "cpc.code")], firms.df[,c("firm.name", "intervention.id")], by = "firm.name")
  int.codes = merge(int.codes, firms.df[,c("intervention.id", "dump.intervention.type")], by = 'intervention.id', all.x = T)
  firms.int.types = merge(codes.last.dumps[, c("firm.name", "BD.intervention.type")], firms.df[,c("firm.name", "intervention.id", 'dump.intervention.type')], by.x = c("firm.name", "BD.intervention.type"), by.y = c("firm.name", "dump.intervention.type"))
  firms.int.types$match = 'by firm and intervention type'
  int.codes = merge(int.codes, unique(firms.int.types[,c("intervention.id", "match")]), by = "intervention.id", all.x = T)
  int.codes[which(is.na(int.codes$match)), "match"] = 'by firm only'

  # We separate the hs/cpc codes in two different data frames
  int.hs.codes = unique(subset(int.codes, select = -c(cpc.code)))
  int.hs.codes = int.hs.codes[!is.na(int.hs.codes$hs.code),]
  int.hs.codes = int.hs.codes[order(int.hs.codes$intervention.id),]

  int.cpc.codes = unique(subset(int.codes, select = -c(hs.code)))
  int.cpc.codes = int.cpc.codes[!is.na(int.cpc.codes$cpc.code),]
  int.cpc.codes = int.cpc.codes[order(int.cpc.codes$intervention.id),]

  # Close he connections
  gta_sql_pool_close()

  return(list(int.hs.codes, int.cpc.codes))
  }
  else {
    print("No hs/cpc codes found for the searched firms")
  }
}
