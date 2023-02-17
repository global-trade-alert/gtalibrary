# Roxygen documentation

#' This function allows you to extract a tailored subset of the GTA database.
#'
#' Make use of the many variables captured in the taxonomy of the Global Trade Alert and tailor the data to your needs. This function returns a list of 2 data frames. The first, master, contains the subset of GTA data you requested. The second, parameter.choices, lists all implicit or exlicited choices that led to the subset.
#'
#' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
#' @param gta.evaluation Specify what GTA evaluations to include. Default is 'any'. Permissible values are 'red', 'amber', 'green' or combinations thereof.
#' @param affected.flows Specify the direction of the trade flow that is affected. The point of view is from the implementing country. Default is 'any'. Permissible values are 'inward', 'outward', 'outward subsidy' or combinations thereof.
#' @param implementing.country Specify the implementing countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.implementer Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated implementing country.
#' @param affected.country Specify the affected countries for your analysis. Default is 'any'. Permissible values are country names or UN codes.
#' @param keep.affected Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated affected country.
#' @param incl.affected.strictness Specify whether to include interventions that affect only one of the selected affected jurisdictions ('ONE'), at least one of the selected affected jurisdictions ('ONEPLUS') or all of the selected affected jurisdictions ('ALL'). Default is 'ONEPLUS'.
#' @param keep.others Specify whether to keep the data for the other jurisdictions that happen to be affected alongside those you specified (T/F). Default is 'TRUE'.
#' @param nr.affected Specify the range for the number of importers affected by an intervention. Default is any number i.e. c(0,999).
#' @param nr.affected.incl Specify whether in the number of importers affected by an intervention is calculated based only on the selected importers are included ('SELECTED'), only on the unselected importers ('UNSELECTED') or based on both ('ALL'). Default is 'ALL'.
#' @param announcement.period Specify a period in which the announcements for your analysis have been made. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions announced since 'after.date'.
#' @param implementation.period Specify a period in which the interventions for your analysis have been implemented. Default is 'any' (incl. not implemented to date). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions implemented since 'after.date'.
#' @param keep.implementation.na Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing implementation.date.
#' @param revocation.period Specify a period in which the interventions for your analysis have been revoked. Default is 'any' (incl. not revoked). Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param submission.period Specify a period in which the interventions for your analysis have been submitted. Default is 'any'. Provide vectors c(after.date, before.date) in R's date format. Also, specify c(after.date, NA) to focus on interventions revoked since 'after.date'.
#' @param keep.revocation.na Specify whether to keep ('TRUE') or remove ('FALSE') interventions with missing revocation.date.
#' @param in.force.on.date Specify the cutoff date to control for in force interventions. Default is the current date (Sys.Date).
#' @param keep.in.force.on.date Specify whether you want to focus on interventions in force on the specified date ('Yes') or no longer in force on the specified date ('No'). Default is 'any' i.e. regardless of enforcement status on the specified date.
#' @param intervention.types Specify the names of the trade policy instruments for your analysis. Default is 'any'. For the permissible values, please see the GTA website or the GTA handbook.
#' @param keep.type Specify whether to focus on ('TRUE') or exclude ('FALSE') interventions with the stated intervention type.
#' @param mast.chapters Specify the MAST chapter IDs for your analysis. Default is 'any'. Permissible values are the MAST chapter letters plus 'tariff', 'fdi', 'migration' and combinations thereof.
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
#' @param keep.interventions Specify whether to focus on ('TRUE') or exclude ('FALSE') the stated intervention IDs.
#' @param lag.adjustment Create a snapshot of the GTA data at the same point in each calendar year since 2009. Specify a cut-off date ('MM-DD').
#' @param reporting.period Specify the period in which an intervention was documented by the GTA team. Default c('2008-11-01',today).
#' @param add.unpublished Including material that is not published (GTA 28 specifc: dumps & subsidies)
#' @param df.name Set the name of the generated result data frame. Default is master.sliced.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.slicer.
#' @param xlsx Takes value TRUE or FALSE. If TRUE, xlsx file will be stored. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.

#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_data_slicer <- function(data = NULL, data.path = "data/master_plus.Rds",
    gta.evaluation = NULL, affected.flows = NULL, implementing.country = NULL,
    keep.implementer = TRUE, affected.country = NULL, keep.affected = NULL,
    incl.affected.strictness = "ONEPLUS", keep.others = TRUE, nr.affected = c(0, 999),
    nr.affected.incl = "ALL", announcement.period = NULL, implementation.period = NULL,
    keep.implementation.na = NULL, revocation.period = NULL, keep.revocation.na = TRUE,
    submission.period = NULL, in.force.on.date = Sys.Date(), keep.in.force.on.date = "any",
    intervention.types = NULL, keep.type = NULL, mast.chapters = NULL, keep.mast = NULL,
    implementation.level = NULL, keep.level = NULL, eligible.firms = NULL,keep.firms = NULL,
    cpc.sectors = NULL, keep.cpc = NULL, hs.codes = NULL, keep.hs = NULL,
    intervention.ids = NULL,keep.interventions = NULL,lag.adjustment = NULL,
    reporting.period = NULL, add.unpublished = FALSE, df.name = "master.sliced",
    pc.name = "parameter.choice.slicer", xlsx = FALSE,output.path = NULL) {

    # data set to NULL --> full data frame can be passed with matriggr or native pipe --> 
    # prevents the function from loading the entire data path every time if it is used in a loop or multiple 
    # times during a session
    if (is.null(data)) data <- load(data.path)

    ### Adding unpublished selection
    if (add.unpublished) {
        library(gtasql)
        library(gtalibrary)
        library(pool)
        library(RMariaDB)
        library(data.table)
        library(gtabastiat)
        library(xlsx)

        gta_sql_kill_connections()

        database <<- "gtamain"

        gta_sql_pool_open(
            db.title = database,
            db.host = gta_pwd(database)$host,
            db.name = gta_pwd(database)$name,
            db.user = gta_pwd(database)$user,
            db.password = gta_pwd(database)$password,
            table.prefix = "gta_"
        )


        master.unpublished <- gta_sql_get_value(
        "SELECT gm.id as state_act_id, gi.id as intervention_id,
        gm.title as title, ge.label as gta_evaluation,
        gj.name as implementing_jurisdiction, gj.un_code as i_un,
        gm.announcement_date as date_announced, gi.inception_date as date_implemented, gi.removal_date as date_removed, gm.creation_date as date_published,
        gmt.name as intervention_type, gil.frontend_label as implementation_level, gef.label as eligible_firms, gaf.label as affected_flow,
        state_act_status_name as current_status
        FROM (SELECT * FROM gta_measure WHERE status_id NOT IN (4,5)) gm
        JOIN (SELECT * FROM gta_intervention gint LEFT JOIN gta_intervention_dump gid ON gint.id = gid.intervention_id WHERE dump_id IS NOT NULL) gi
        ON gm.id= gi.measure_id
        JOIN gta_state_act_status_list gsasl
        ON gm.status_id = gsasl.state_act_status_id
        JOIN gta_measure_type gmt
        ON gi.measure_type_id = gmt.id
        JOIN gta_evaluation ge
        ON gi.evaluation_id = ge.id
        JOIN gta_affected_flow gaf
        ON gi.affected_flow_id = gaf.id
        JOIN gta_implementation_level gil
        ON gi.implementation_level_id = gil.id
        JOIN gta_eligible_firms gef
        ON gi.eligible_firms_id = gef.id
        JOIN gta_implementing_jurisdiction gij
        ON gi.id = gij.intervention_id
        JOIN gta_jurisdiction gj
        ON gij.jurisdiction_id = gj.id")

        ## sectors, products, AJ
        my.ints <- unique(master.unpublished$intervention.id)
        gta_tuple <- gta_sql_get_value(paste0("SELECT DISTINCT intervention_id, sector_code_3 as sector_code, tariff_line_id, gj.un_code as un_code_implementer, gj2.un_code as un_code_affected
                                    FROM (SELECT *
                                    FROM gta_it_revised WHERE intervention_id IN (", paste(my.ints, collapse = ","), ")) git
                                    JOIN gta_jurisdiction gj
                                    ON git.implementing_jurisdiction_id = gj.id
                                    JOIN gta_jurisdiction gj2
                                    ON git.affected_jurisdiction_id = gj2.id;"))

        ## gta_tariff_line
        gta_tariff_line <- gta_sql_get_value(paste0("SELECT * FROM gta_tariff_line;"))
        data.table::setnames(gta_tariff_line, old = "id", new = "tariff.line.id")
        data.table::setnames(gta_tariff_line, old = "code", new = "affected.products")

        gta_tuple <- merge(gta_tuple, gta_tariff_line[, c("tariff.line.id", "affected.products")], by = "tariff.line.id", all.x = T)
        gta_tuple$tariff_line_id <- NULL

        ## gta_affected_tariff_line
        gta_affected_tariff_line <- gta_sql_get_value(paste0("SELECT * FROM gta_affected_tariff_line WHERE intervention_id IN (", paste(my.ints, collapse = ","), ");"))
        setnames(gta_affected_tariff_line, old = "tariff.line.code", "affected.products")

        ## where we have support tables, we have it in gta_tuple
        master.tuple <- subset(master.unpublished, intervention.id %in% gta_tuple$intervention.id)
        master.else <- subset(master.unpublished, !intervention.id %in% gta_tuple$intervention.id)

        ## correcting for zeroes at the start of gta_tuple
        gta_tuple$l.hs <- nchar(gta_tuple$affected.products)
        gta_tuple$l.cpc <- nchar(gta_tuple$sector.code)

        gta_tuple$affected.products <- as.character(gta_tuple$affected.products)
        gta_tuple$sector.code <- as.character(gta_tuple$sector.code)

        gta_tuple$sector_code[gta_tuple$l.cpc == 2 & is.na(gta_tuple$l.cpc) == F] <- paste("0", gta_tuple$sector.code[gta_tuple$l.cpc == 2 & is.na(gta_tuple$l.cpc) == F], sep = "")
        gta_tuple$affected_products[gta_tuple$l.hs == 5 & is.na(gta_tuple$l.hs) == F] <- paste("0", gta_tuple$affected.products[gta_tuple$l.hs == 5 & is.na(gta_tuple$l.hs) == F], sep = "")

        gta_tuple$l.hs <- NULL
        gta_tuple$l.cpc <- NULL

        ## the rest goes through the other gta_ tables.
        ## First line creates a table of all combinations, 2nd adds sectors (where available), and last products (where available).
        all <- unique(gta_tuple[, c("intervention.id", "un.code.implementer", "un.code.affected")])

        cpc.hs <- merge(aggregate(sector.code ~ intervention.id + un.code.implementer + un.code.affected, gta_tuple, function(x) paste(unique(x), collapse = ", ")),
            aggregate(affected.products ~ intervention.id + un.code.implementer + un.code.affected, gta_tuple, function(x) paste(unique(x), collapse = ", ")),
            by = c("intervention.id", "un.code.implementer", "un.code.affected"), all.x = T
        )
        cpc.hs <- merge(all, cpc.hs, by = c("intervention.id", "un.code.implementer", "un.code.affected"), all.x = TRUE)

        ## should be =1
        length(unique(cpc.hs$intervention.id)) / length(unique(master.tuple$intervention.id))

        setnames(cpc.hs, old = "affected.products", "affected.product")
        setnames(cpc.hs, old = "sector.code", "affected.sector")
        setnames(cpc.hs, old = "un.code.affected", "a.un")
        setnames(cpc.hs, old = "un.code.implementer", "i.un")
        setnames(cpc.hs, old = "intervention.id", "intervention.id")

        master.tuple <- merge(master.tuple, cpc.hs, by = c("intervention.id", "i.un"), all.x = TRUE)

        ## the non-tuple cases
        gta_affected_sector <- gta_sql_get_value(paste0("SELECT * FROM gta_affected_sector WHERE intervention_id IN (", paste(my.ints, collapse = ","), ");"))
        gta_affected_sector$type <- NULL
        setnames(gta_affected_sector, old = "sector.code", "affected.sector")

        setnames(gta_affected_tariff_line, old = "affected.products", "affected.product")

        gta_affected_jurisdiction <- gta_sql_get_value(paste0("SELECT gaj.intervention_id, un_code as a_un, name as affected_jurisdiction
                                                   FROM (SELECT * FROM gta_affected_jurisdiction WHERE type != 'D' AND intervention_id IN (", paste(my.ints, collapse = ","), ")) gaj
                                                   JOIN gta_jurisdiction gj
                                                   ON gaj.jurisdiction_id = gj.id;"))

        ## correcting for zeroes at the start of gta_tuple
        gta_affected_tariff_line$l.hs <- nchar(gta_affected_tariff_line$affected.product)
        gta_affected_tariff_line$affected.product <- as.character(gta_affected_tariff_line$affected.product)
        gta_affected_tariff_line$affected.product[gta_affected_tariff_line$l.hs == 5 & is.na(gta_affected_tariff_line$l.hs) == F] <- paste("0", gta_affected_tariff_line$affected.product[gta_affected_tariff_line$l.hs == 5 & is.na(gta_affected_tariff_line$l.hs) == F], sep = "")
        gta_affected_tariff_line$l.hs <- NULL

        gta_affected_sector$l.cpc <- nchar(gta_affected_sector$affected.sector)
        gta_affected_sector$affected.sector <- as.character(gta_affected_sector$affected.sector)
        gta_affected_sector$affected.sector[gta_affected_sector$l.cpc == 2 & is.na(gta_affected_sector$l.cpc) == F] <- paste("0", gta_affected_sector$affected.sector[gta_affected_sector$l.cpc == 2 & is.na(gta_affected_sector$l.cpc) == F], sep = "")
        gta_affected_sector$l.cpc <- NULL

        master.else <- merge(master.else,
            aggregate(affected.sector ~ intervention.id, gta_affected_sector, function(x) paste(unique(x), collapse = ", ")),
            by = "intervention.id", all.x = TRUE
        )

        master.else <- merge(master.else,
            aggregate(affected.product ~ intervention.id, gta_affected_tariff_line, function(x) paste(unique(x), collapse = ", ")),
            by = "intervention.id", all.x = TRUE
        )

        master.else <- merge(master.else,
            gta_affected_jurisdiction,
            by = "intervention.id", all.x = TRUE
        )

        master.tuple <- merge(master.tuple, unique(gta_affected_jurisdiction[, c("a.un", "affected.jurisdiction")]), by = "a.un", all.x = T)
        length(unique(rbind(master.tuple, master.else)$intervention.id)) / length(unique(master.unpublished$intervention.id))

        master.unpublished <- rbind(master.tuple, master.else)

        rm(gta_tariff_line, gta_affected_sector, gta_affected_tariff_line, gta_affected_jurisdiction, cpc.hs, all, my.ints, gta_tuple)

        gta_sql_pool_close()
        gta_sql_kill_connections()

        ## formatting corrections
        master.unpublished$date.announced <- as.Date(master.unpublished$date.announced)
        master.unpublished$date.implemented <- as.Date(master.unpublished$date.implemented)
        master.unpublished$date.removed <- as.Date(master.unpublished$date.removed)
        master.unpublished$affected.flow <- tolower(master.unpublished$affected.flow)

        today <- Sys.Date()
        master.unpublished$currently.in.force <- "No"
        master.unpublished$currently.in.force[master.unpublished$date.implemented <= today & (master.unpublished$date.removed > today | is.na(master.unpublished$date.removed) == T)] <- "Yes"

        mt <- gtalibrary::int.mast.types[, c("intervention.type", "mast.chapter.id", "mast.subchapter.id")]
        names(mt) <- c("intervention.type", "mast.chapter", "mast.id")
        mt$intervention.type <- as.character(mt$intervention.type)

        master.unpublished <- merge(master.unpublished, mt[, c("intervention.type", "mast.id", "mast.chapter")], by = "intervention.type", all.x = T)

        country.iso <- gtalibrary::country.names
        master.unpublished$i.atleastone.G20 <- as.numeric(master.unpublished$intervention.id %in% subset(master.unpublished, i.un %in% subset(country.iso, is.g20 == T)$un_code)$intervention.id)
        master.unpublished$a.atleastone.G20 <- as.numeric(master.unpublished$intervention.id %in% subset(master.unpublished, a.un %in% subset(country.iso, is.g20 == T)$un_code)$intervention.id)
        rm(country.iso)

        ## combining DFs
        master$current.status <- "published"
        master <- unique(rbind(
            master,
            subset(master.unpublished, !intervention.id %in% unique(master$intervention.id))
        ))

        rm(mt, master.unpublished, master.tuple, master.else)
        parameter.choices <- rbind(
            parameter.choices,
            data.frame(parameter = "Unpublished:", choice = "Included")
        )
    } else {
        parameter.choices <- rbind(
            parameter.choices,
            data.frame(parameter = "Unpublished:", choice = "Excluded")
        )
    }



##################################################################
# REWRITTEN PART OF THE FUNCTION!!!
##################################################################

##################################################################
##### Check validity of input arguments (no filtering of data frame yet --> can all be done in one pipe!!! )
##################################################################
filter_statement <- vector(mode = "character")
# first & in filter statement must be cut out first!!!!

    # gta.evaluation
    if (!is.null(gta.evaluation)) {
        gta_parameter_check(check.name = "gta.evaluation", tolower(gta.evaluation), c("red", "amber", "green"))
        gta_evaluatioan <- str_to_title(gta.evaluation)
        filter_statement <- append(filter_statement, "gta.evaluation %in% gta_evaluation")
    }

    # affected flows
    if (!is.null(affected.flows)) {
        gta_parameter_check(check.name = "affected.flows", tolower(affected.flows), c("inward", "outward", "outward subsidy"))
        filter_statement <- append(filter_statement, "affected.flow %in% affected.flows")
    }

    # check implementing country
    if (!is.null(implementing_country)){
        implementing.country <- gtalibrary::gta_un_code_vector(countries = implementing.country)
        filter_statement <- append(filter_statement, "i.un %in% implementing_country")
        if (!keep.implementer){
            filter_statement <- append(filter_statement, "!i.un %in% implementing_country")
        }
    }

    # check affected.country
    if (!is.null(affected.country)) {
        gtalibrary::gta_type_check(keep.affected, is.logical)
        affected.country <- gtalibrary::gta_un_code_vector(countries = affected.country)
        filter_statement <- append(filter_statement, "a.un %in% affected_country")

        if (!keep.affected){
            filter_statement <- append(filter_statement, "!a.un %in% affected_country")
        }
    }

    # intervention.types
    if (!is.null(intervention.types)) {
        # if intervention.types are specified, the parameter keep.type must also be specified
        gtalibrary::gta_type_check(keep.type, is.logical)
        admissible.values <- tolower(gtalibrary::int.mast.types$intervention.type)
        gtalibrary::gta_parameter_check(check.name = "intervention.types", intervention.types, permissible.values = admissible.values)
        filter_statement <- append(filter_statement, "intervention.type %in% intervention.types")
        
        if (!keep.type) {
            filter_statement <- append(filter_statement, "!intervention.type %in% intervention.types")
        }
    }

    # mast.chapters
    if (!is.null(mast.chapters)) {
        gtalibrary::gta_type_check(keep.mast, is.logical)
        mast.chapters <- stringr::str_remove_all(pattern = "[0-9]+", string = mast.chapters)
        admissible.values <- tolower(gtalibrary::int.mast.types$mast.chapter.id)
        filter_statement <- append(filter_statement, "mast.id %in% mast.chapters")
        
        if (!keep.mast) {
            filter_statement <- append(filter_statement, "!mast.id %in% mast.chapters")
        }
    }

   # eligible.firms
    if (!is.null(eligible.firms)) {
        # check if keep.affected is specified
        gtalibrary::gta_type_check(keep.firms, is.logical)
        admissible.values <- tolower(gtalibrary::elig.firms$eligible.firms)
        gtalibrary::gta_parameter_check(check.name = "eligible.firms", tolower(eligible.firms), admissible.values)
        filter_statement <- append(filter_statement, "eligible.firms %in% eligible_firms")
        
        if (!keep.firms) {
            filter_statement <- append(filter_statement, "eligible.firms %in% eligible_firms")
        }
    }

    # implementation.level
    if (!is.null(implementation.level)) {
        # check if keep.affected is specified
        gtalibrary::gta_type_check(keep.level, is.logical)

        admissible.values <- tolower(gtalibrary::imp.levels$implementation.level)
        gtalibrary::gta_parameter_check(check.name = "implementation.level", tolower(implementation.level), admissible.values)
        filter_impleentationlevel <- "implementation.level %in% implementation.level"

        if (!keep.level) {
            filter_implementationlevel <- "!implementation.level %in% implementation.level"
        }
    }

    # intervention.ids
    if (!is.null(intervention.ids)) {
        # if intervention.types are specified, the parameter keep.interventions must also be specified
        gta_type_check(keep.interventions, is.logical)
        admissible.values <- unique(master$intervention.id)
        gta_parameter_check(check.name = "intervention.id", nr.affected.incl, permissible.values = admissible.values)
        filter_statement <- append(filter_statement, "intervention.id %in% intervention.ids")
        if (!keep.interventions) {
        filter_statement <- append(filter_statement, "!intervention.id %in% intervention.ids")
        }
    }

# filter the data frame for the first time
    filter_statement <- paste(filter_statement, collapse = " & ")
    data <- dtplyr::lazy_dt(master) |>
        dplyr::filter(eval(parse(text = filter_statement))) |>
        tibble::as_tibble()

    # hs codes
    if (!is.null(hs.codes)) {
        gtalibrary::gta_type_check(keep.hs, is.logical)
        hs.codes  <- gta_hs_code_check(codes = hs.codes)
        filter_hs <- "hs.codes %in% hs.codes"

        if (!keep.hs) {
            filter_hs <- "!hs.codes %in% hs.codes"
        }

        data <- dtplyr::lazy_dt(data) |>
            mutate(
                ids = seq_len(nrow(data)),
                hs_codes = str_split(string = affected.product, pattern = ", ")
            ) |>
            tibble::as_tibble() |>
            tidyr::unnest(cols = hs_codes) |>
            dplyr::filter(hs_codes = eval(parse(text = filter_hs)))
            group_by(ids) |> #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            summarize(...) ###### collapse values again by IDs!!!!
    }

    # what if cpc and hs sectors are selected ? ??
    # cpc.sectors
    if (!is.null(cpc.sectors)) {
        # do from filtered --> merged
        gtalibrary::gta_type_check(keep.cpc, is.logical)
        cpc.sectors <- gtalibrary::gta_cpc_code_check(codes = cpc.sectors)
        filter_cpc <- "affected.sector %in% cpc.sectors"

        if (!keep.cpc) {
            filter_cpc <- "!affected.sector %in% cpc.sectors"
        }

        data <- dtplyr::lazy_dt(data) |>
            mutate(
                ids = seq_len(nrow(data)),
                cpc_codes = str_split(string = affected.sector, pattern = ", ")
            ) |>
            tibble::as_tibble() |>
            tidyr::unnest(cols = cpc_codes) |>
            dplyr::filter(cpc_codes = eval(parse(text = filter_cpc))) |> 
            group_by(ids) |> 
            summarize(...) ###### collapse values again by IDs!!!!
    }

    # check nr.affected.incl
    gtalibrary::gta_parameter_check(check.name = "nr.affected.incl", tolower(nr.affected.incl)), permissible.values = c("all", "selected", "unselected")

    # incl. affected strictness
    gtalibrary::gta_parameter_check(check.name = "incl.affected.strictness", tolower(incl.affected.strictness), c("all", "one", "oneplus"))

    # implementation.period
    if (!is.null(implementation.period)) {
        gtalibrary::gta_type_check(implementation.period, lubridate::is.Date)
        gtalibrary::gta_type_check(keep.implementation.na, is.logical)
    }

    # submission.period
    if (!is.null(submission.period)){

    }

    # revocation.period
    if (!is.null(revocation.period)) {

    }

    # reporting.period
    if (!is.null(reporting.period)) {

    }

    # lag adjustment
    if(!is.null(lag.adjustment)){

    }

    # check logical values / prepopulated
    gtalibrary::gta_type_check(keep.implementer, is.logical)
    gtalibrary::gta_type_check(keep.revocation.na, is.logical)
    gtalibrary::gta_type_check(keep.others, is.logical)
    gtalibrary::gta_type_check(add.unpublished, is.logical)
    gtalibrary::gta_type_check(xlsx, is.logical)


    affected.interventions <- master$intervention.id ### WHAT DO WE DO WITH THIS?????????????


    affected.interventions <- subset(master, a.un %in% affected)$intervention.id
                }

            # WHAT TO DO WITH THIS --> REFERS TO INCL. AFFECTED.STRICTNESS --> SEEMS to be time-costly operation because of the loop! --> Many function calls
            else {
                ## this is the 'ONEPLUS' case.
                affected.combinations <- unique(master$intervention.id)

                if (incl.affected.strictness == "ALL") {
                    for (cty in affected) {
                        a.c.temp <- unique(subset(master, a.un == cty)$intervention.id)

                        affected.combinations <- intersect(
                            affected.combinations,
                            a.c.temp
                        )
                        rm(a.c.temp)
                    }
                }

                if (incl.affected.strictness == "ONE") {
                    one.aj <- subset(master, a.un %in% affected)

                    one.aj <- aggregate(a.un ~ intervention.id, one.aj, function(x) length(unique(x)))

                    affected.combinations <- subset(one.aj, a.un == 1)$intervention.id

                    rm(one.aj)
                }

            affected.interventions <- intersect(affected.interventions, affected.combinations)

            ### applying the restrictions on the number of affected trading partners per intervention

            ## min/max nr of affected jurisdictions
            nr.affected.min <- nr.affected[1]
            nr.affected.max <- nr.affected[2]


            ## Calculation form
            if (!nr.affected.incl %in% c("ALL", "SELECTED", "UNSELECTED")) {
                stop.print <- "Please choose which imports to include into the calculation for the number of affected jurisdictions (ALL/SELECTED/UNSELECTED)."
                error.message <<- c(T, stop.print)
                stop(stop.print)
            } else {
                if (nr.affected.incl == "ALL") {
                    imp.count <- aggregate(a.un ~ intervention.id, master, function(x) length(unique(x)))

                    parameter.choices <- rbind(
                        parameter.choices,
                        data.frame(parameter = "Nr. of affected jurisdictions calculated based on: ", choice = "All affected jurisdictions")
                    )
                }

                if (nr.affected.incl == "SELECTED") {
                    imp.count <- aggregate(a.un ~ intervention.id, subset(master, a.un %in% affected), function(x) length(unique(x)))

                    parameter.choices <- rbind(
                        parameter.choices,
                        data.frame(parameter = "Nr. of affected jurisdictions calculated based on: ", choice = "Selected affected jurisdictions")
                    )
                }

                if (nr.affected.incl == "UNSELECTED") {
                    imp.count <- aggregate(a.un ~ intervention.id, subset(master, !a.un %in% affected), function(x) length(unique(x)))


                    parameter.choices <- rbind(
                        parameter.choices,
                        data.frame(parameter = "Nr. of affected jurisdictions calculated based on: ", choice = "Unselected affected jurisdictions")
                    )
                }

                # ensuring zero-AJ interventions are counted
                imp.count <- rbind(
                    imp.count,
                    data.frame(
                        intervention.id = unique(master$intervention.id[is.na(master$a.un)]),
                        a.un = 0
                    )
                )

                affected.interventions <- intersect(
                    affected.interventions,
                    subset(imp.count, a.un >= nr.affected.min &
                        a.un <= nr.affected.max)$intervention.id
                )
            }

            ## applying all these conditions
            master <- subset(master, intervention.id %in% affected.interventions)

            ## keep.others
            if (keep.others == FALSE & is.null(affected.country) == F) {
                master <- subset(master, a.un %in% affected)

            # announcement.period
            date.period <- announcement.period

            } else {
                if (length(date.period) != 2) {
                    stop.print <- "Please specify the date pair (after.date, before.date) for the announcement period. 'NA' is permissible, but has to be specified in case you only want one of the two."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                } else {
                    dates <- sum(as.numeric(is.na(date.period)) == F)

                    if (dates > 0) {
                        if (sum(is.na(as.Date(date.period[is.na(date.period) == F], "%Y-%m-%d"))) > 0) {
                            error.message <<- c(T, "At least one of the announcement dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
                            stop("At least one of the announcement dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
                        }

                        if (dates == 2) {
                            master <- subset(master, date.announced >= as.Date(date.period[1], "%Y-%m-%d") & date.announced <= as.Date(date.period[2], "%Y-%m-%d"))

                        }

                        if (dates == 1) {
                            if (is.na(as.Date(date.period[1], "%Y-%m-%d")) == F) {
                                master <- subset(master, date.announced >= as.Date(date.period[1], "%Y-%m-%d"))

                            }

                            if (is.na(as.Date(date.period[2], "%Y-%m-%d")) == F) {
                                master <- subset(master, date.announced <= as.Date(date.period[2], "%Y-%m-%d"))

                            }
                        }

                    }
                }

                remove(date.period)
            }

            # implementation.period
            date.period <- implementation.period

            if (is.null(date.period)) {
                if (is.null(keep.implementation.na) == F) {
                    if (keep.implementation.na == F) {
                        master <- subset(master, is.na(date.implemented) == F)

            } else {
                if (length(date.period) != 2) {
                    stop.print <- "Please specify the date pair (after.date, before.date) for the implementation period. 'NA' is permissible, but has to be specified in case you only want one of the two."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                } else {
                    dates <- sum(as.numeric(is.na(date.period)) == F)

                    if (dates > 0) {
                        if (sum(is.na(as.Date(date.period[is.na(date.period) == F], "%Y-%m-%d"))) > 0) {
                            stop("At least one of the implementation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
                        }

                        if (is.null(keep.implementation.na)) {
                            stop.print <- "Please specify whether you want to keep interventions with missing implementation date or exclude them (keep.implementation.na=T/F)."
                            error.message <<- c(T, stop.print)
                            stop(stop.print)
                        }

                        if (dates == 2) {
                            if (keep.implementation.na == T) {
                                master <- subset(master, (date.implemented >= as.Date(date.period[1], "%Y-%m-%d") & date.implemented <= as.Date(date.period[2], "%Y-%m-%d")) | is.na(date.implemented) == T)
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "implementation period:", choice = paste(date.period[1], " - ", date.period[2], ", as well as interventions without implementation date", sep = ""))
                                )
                            }

                            if (keep.implementation.na == F) {
                                master <- subset(master, date.implemented >= as.Date(date.period[1], "%Y-%m-%d") & date.implemented <= as.Date(date.period[2], "%Y-%m-%d"))
                        }

                        if (dates == 1) {
                            if (is.na(as.Date(date.period[1], "%Y-%m-%d")) == F) {
                                if (keep.implementation.na == T) {
                                    master <- subset(master, date.implemented >= as.Date(date.period[1], "%Y-%m-%d") | is.na(date.implemented) == T)
                                }
                                if (keep.implementation.na == F) {
                                    master <- subset(master, date.implemented >= as.Date(date.period[1], "%Y-%m-%d"))
                                }
                            }

                            if (is.na(as.Date(date.period[2], "%Y-%m-%d")) == F) {
                                if (keep.implementation.na == T) {
                                    master <- subset(master, date.implemented <= as.Date(date.period[2], "%Y-%m-%d") | is.na(date.implemented) == T)
                                }
                                if (keep.implementation.na == F) {
                                    master <- subset(master, date.implemented <= as.Date(date.period[2], "%Y-%m-%d"))
                                }
                            }
                        }

                remove(date.period)
            }

            # revocation.period
            date.period <- revocation.period

            if (is.null(date.period)) {
                parameter.choices <- rbind(
                    parameter.choices,
                    data.frame(parameter = "Revocation period:", choice = "None specified")
                )
            } else {
                if (length(date.period) != 2) {
                    stop.print <- "Please specify the date pair (after.date, before.date) for the revocation period. 'NA' is permissible, but has to be specified in case you only want one of the two."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                } else {
                    dates <- sum(as.numeric(is.na(date.period)) == F)

                    if (dates > 0) {
                        if (sum(is.na(as.Date(date.period[is.na(date.period) == F], "%Y-%m-%d"))) > 0) {
                            error.message <<- c(T, "At least one of the revocation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
                            stop("At least one of the revocation dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'.")
                        }

                        if (is.null(keep.revocation.na)) {
                            stop.print <- "Please specify whether you want to keep interventions with missing revocation date or exclude them (keep.revocation.na=T/F)."
                            error.message <<- c(T, stop.print)
                            stop(stop.print)
                        }

                        if (dates == 2) {
                            if (keep.revocation.na == T) {
                                master <- subset(master, (date.removed >= as.Date(date.period[1], "%Y-%m-%d") & date.removed <= as.Date(date.period[2], "%Y-%m-%d")) | is.na(date.removed) == T)
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Revocation period:", choice = paste(date.period[1], " - ", date.period[2], ", as well as interventions without removal date", sep = ""))
                                )
                            }

                            if (keep.revocation.na == F) {
                                master <- subset(master, date.removed >= as.Date(date.period[1], "%Y-%m-%d") & date.removed <= as.Date(date.period[2], "%Y-%m-%d"))
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Revocation period:", choice = paste(date.period[1], " - ", date.period[2], " and excluding interventions without removal date", sep = ""))
                                )
                            }
                        }

                        if (dates == 1) {
                            if (is.na(as.Date(date.period[1], "%Y-%m-%d")) == F) {
                                if (keep.revocation.na == T) {
                                    master <- subset(master, date.removed >= as.Date(date.period[1], "%Y-%m-%d") | is.na(date.removed) == T)
                                }
                                if (keep.revocation.na == F) {
                                    master <- subset(master, date.removed >= as.Date(date.period[1], "%Y-%m-%d"))
                                }
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Revocation period:", choice = paste(date.period[1], " or more recent", sep = ""))
                                )
                            }

                            if (is.na(as.Date(date.period[2], "%Y-%m-%d")) == F) {
                                if (keep.revocation.na == T) {
                                    master <- subset(master, date.removed <= as.Date(date.period[2], "%Y-%m-%d") | is.na(date.removed) == T)
                                }
                                if (keep.revocation.na == F) {
                                    master <- subset(master, date.removed <= as.Date(date.period[2], "%Y-%m-%d"))
                                }
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Revocation period:", choice = paste(date.period[2], " or earlier", sep = ""))
                                )
                            }
                        }
                    } else {
                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = "Revocation period:", choice = "None specified")
                        )
                    }
                }

                remove(date.period)
            }

            # submission.period
            date.period <- submission.period

            if (is.null(date.period)) {
                parameter.choices <- rbind(
                    parameter.choices,
                    data.frame(parameter = "Submission period:", choice = "Full GTA monitoring period")
                )
            } else {
                if (length(date.period) != 2) {
                    stop.print <- "Please specify the date pair (after.date, before.date) for the submission period. 'NA' is permissible, but has to be specified in case you only want one of the two."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                } else {
                    dates <- sum(as.numeric(is.na(date.period)) == F)

                    if (dates > 0) {
                        if (sum(is.na(as.Date(date.period[is.na(date.period) == F], "%Y-%m-%d"))) > 0) {
                            stop.print <- "At least one of the submission dates you specified is neither in R date format ('2008-12-31'), nor specified as 'NA'."
                            error.message <<- c(T, stop.print)
                            stop(stop.print)
                        }

                        if (dates == 2) {
                            master <- subset(master, date.published >= as.Date(date.period[1], "%Y-%m-%d") & date.published <= as.Date(date.period[2], "%Y-%m-%d"))
                            parameter.choices <- rbind(
                                parameter.choices,
                                data.frame(parameter = "Submission period:", choice = paste(date.period[1], " - ", date.period[2], sep = ""))
                            )
                        }

                        if (dates == 1) {
                            if (is.na(as.Date(date.period[1], "%Y-%m-%d")) == F) {
                                master <- subset(master, date.published >= as.Date(date.period[1], "%Y-%m-%d"))
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Submission period:", choice = paste(date.period[1], " or more recent", sep = ""))
                                )
                            }

                            if (is.na(as.Date(date.period[2], "%Y-%m-%d")) == F) {
                                master <- subset(master, date.published <= as.Date(date.period[2], "%Y-%m-%d"))
                                parameter.choices <- rbind(
                                    parameter.choices,
                                    data.frame(parameter = "Submission period:", choice = paste(date.period[2], " or earlier", sep = ""))
                                )
                            }
                        }
                    } else {
                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = "Submission period:", choice = "Full GTA monitoring period")
                        )
                    }
                }

                remove(date.period)
            }

            # in.force.on.date
            if (is.null(in.force.on.date) & keep.in.force.on.date == "any") {
                in.force.on.date <- Sys.Date()
                parameter.choices <- rbind(
                    parameter.choices,
                    data.frame(parameter = paste0("In force on ", in.force.on.date, ":"), choice = "Regardless")
                )
            } else {
                # determine interpretability of in.force.on.date
                tryCatch(
                    {
                        date <- format(as.Date(x = in.force.on.date), "%Y-%m-%d")
                    },
                    error = function(e) {
                        stop.print <- "Please specify a valid unique in.force.on.date ('yyyy-mm-dd'). Default is current date (Sys.Date)."
                        error.message <<- c(T, stop.print)
                        stop(stop.print)
                    },
                    finally = {
                        if (is.na(date) | !substr(as.character(date), 1, 4) %in% as.character(2000:2024) | length(date) != 1) {
                            stop.print <- "Please specify a valid unique in.force.on.date ('yyyy-mm-dd'). Default is current date (Sys.Date)."
                            error.message <<- c(T, stop.print)
                            stop(stop.print)
                        }
                    }
                )
                in.force.on.date <- as.Date(x = in.force.on.date)

                if (tolower(keep.in.force.on.date) %in% c("yes", "no", "any")) {
                    if (tolower(keep.in.force.on.date) == "any") {
                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = paste0("In force on ", in.force.on.date, ":"), choice = "Regardless")
                        )
                    }

                    if (tolower(keep.in.force.on.date) == "yes") {
                        master <- subset(master, date.implemented <= in.force.on.date & (is.na(date.removed) == T | date.removed >= in.force.on.date))

                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = paste0("In force on ", in.force.on.date, ":"), choice = "Yes")
                        )
                    }

                    if (tolower(keep.in.force.on.date) == "no") {
                        master <- subset(master, (date.implemented < in.force.on.date & is.na(date.removed) == F & date.removed < in.force.on.date) | is.na(date.implemented))

                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = paste0("In force on ", in.force.on.date, ":"), choice = "No")
                        )
                    }
                } else {
                    stop.print <- "Please specify keep.in.force.on.date as either 'yes', 'no' or 'any'."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                }
            }

## Correcting the affected product column to only include HS codes belong to the cpc.sectors, if any.
                    if (min(cpc.sectors) < 500) {
                        if (keep.cpc == T) {
                            products <- gta_cpc_to_hs(cpc.sectors[cpc.sectors < 500])
                        } else {
                            not.mentioned.cpc <- unique(gtalibrary::cpc.to.hs$cpc)[!unique(gtalibrary::cpc.to.hs$cpc) %in% cpc.sectors]
                            products <- gta_cpc_to_hs(not.mentioned.cpc)
                        }

                        # Create new specific id and master.temp
                        master$new.id <- seq(1, nrow(master))
                        master.temp <- unique(master[, c("new.id", "affected.product")])
                        master.temp <- cSplit(master.temp, which(colnames(master.temp) == "affected.product"), direction = "long", sep = ",")

                        master.temp <- subset(master.temp, affected.product %in% products)

                        # clear affected.product/affected.sector column
                        master$affected.product <- NULL


                        # Collapse hs codes by id
                        master.hs <- aggregate(. ~ new.id, master.temp, function(x) toString(x))

                        # Merge and remove new.id
                        master <- merge(master, master.hs, by = "new.id", all.x = T) # all.x=T is vital here since there may also be service sectors in cpc.sectors
                        master$new.id <- NULL
                    } else {
                        ## If the stated sectors only include services, then remove all HS codes that may also have been affected by the same intervention
                        master$affected.product <- NA
                    }
                }
            }

            # hs.codes
            # keep.hs
                    # HS code check
                    hs.codes <- gta_hs_code_check(codes = hs.codes)

                    # Create new specific id and master.temp
                    master$new.id <- seq(1, nrow(master))
                    master.temp <- unique(master[, c("new.id", "affected.product")])
                    master.temp <- cSplit(master.temp, which(colnames(master.temp) == "affected.product"), direction = "long", sep = ",")

                    # Filter products
                    if (keep.hs == T) {
                        master.temp <- subset(master.temp, affected.product %in% hs.codes)

                        parameter.choices <- rbind(
                            parameter.choices,
                            data.frame(parameter = "HS codes included:", choice = paste(hs.codes, collapse = ", "))
                        )
                    } else if (keep.hs == F) {
                        master.temp <- subset(master.temp, !affected.product %in% hs.codes)

                    }

                    # clear affected.product/affected.sector column
                    master$affected.product <- NULL
                    master$affected.sector <- NULL

                    # Collapse hs codes by id
                    master.hs <- aggregate(. ~ new.id, master.temp, function(x) toString(x))

                    # Add and collapse corresponding CPC codes
                    cpc <- gtalibrary::cpc.to.hs
                    cpc$affected.product <- cpc$hs
                    cpc$affected.sector <- cpc$cpc
                    master.temp <- merge(master.temp, cpc, by = "affected.product", all.x = T)

                    # Check # of rows
                    if (nrow(master.temp) == 0) {
                        stop.print <- "Unfortunately no rows remaining while filtering for hs.codes"
                        error.message <<- c(T, stop.print)
                        stop(stop.print)
                    }
                    master.cpc <- aggregate(affected.sector ~ new.id, master.temp, function(x) toString(unique(x)))
                    cpc$affected.product <- NULL
                    cpc$affected.sector <- NULL

                    # Merge and remove new.id
                    master <- merge(master, master.hs, by = "new.id")
                    master <- merge(master, master.cpc, by = "new.id")
                    master$new.id <- NULL
                }
            }

            # reporting lag adjustment
            if (is.null(lag.adjustment)) {
                parameter.choices <- rbind(
                    parameter.choices,
                    data.frame(parameter = "Lag adjustment:", choice = "Unadjusted")
                )
            } else {
                if (is.na(as.Date(lag.adjustment, "%m-%d")) == T) {
                    stop.print <- "Please specifiy a valid lag date ('mm-dd')."
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                } else {
                    # Remove interventions without implementation date
                    master <- subset(master, is.na(date.implemented) == F)

                    # set lag date
                    master$date.lag <- as.Date(paste(data.table::year(master$date.implemented), lag.adjustment, sep = "-"), "%Y-%m-%d")
                    master <- subset(master, date.published <= date.lag)
                    master$date.lag <- NULL


            # reporting period
            if (is.null(reporting.period)) {
                parameter.choices <- rbind(
                    parameter.choices,
                    data.frame(parameter = "Reporting period:", choice = "Complete GTA monitoring period")
                )
            } else {
                report.start <- as.Date(reporting.period[1], "%Y-%m-%d")
                report.end <- as.Date(reporting.period[2], "%Y-%m-%d")

                if (is.na(report.start) | is.na(report.end)) {
                    stop.print <- "Reporting period does not correspond to format YYY-MM-DD"
                    error.message <<- c(T, stop.print)
                    stop(stop.print)
                }

                master <- subset(master, date.published >= report.start & date.published <= report.end)

            }

            ## writing to disk
            if (xlsx == T) {
                print("Saving XLSX ...")
                if (is.null(output.path)) {
                    write.xlsx(master, file = paste("GTA data slicer output ", Sys.Date(), ".xlsx", sep = ""), sheetName = "Interventions", row.names = F)
                    write.xlsx(parameter.choices, file = paste("GTA data slicer output ", Sys.Date(), ".xlsx", sep = ""), sheetName = "Parameter choices", row.names = F, append = T)
                    print("Saving XLSX ... completed in working directory")
                } else {
                    write.xlsx(trade.coverage.estimates, file = output.path, sheetName = "Estimates", row.names = F)
                    write.xlsx(parameter.choices, file = output.path, sheetName = "Parameter choices", row.names = F, append = T)
                    print("Saving XLSX ... completed in output path")
                }
            }

# must both be valid at the same time ? --> Yes!
gtalibrary::gta_data_slicer(hs.codes = c(871200), keep.hs = TRUE, cpc.sectors = 268, keep.cpc = TRUE, intervention.ids = c(57647, 58547, 60142, 101832, 58547, 87241))

