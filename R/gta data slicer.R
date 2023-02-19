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
#' @param add.unpublished Including material that is not published (GTA 28 specifc: dumps & subsidies)
#' @param df.name Set the name of the generated result data frame. Default is master.sliced.
#' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.slicer.
#' @param xlsx Takes value TRUE or FALSE. If TRUE, xlsx file will be stored. Default: FALSE
#' @param output.path Takes the value of the output path (without the filename) added to the working directory as a string starting with "/". Default: None.
#' @import data.table
#' @import dtplyr
#' @import tidyr
#' @import tibble
#' @import dplyr
#' @import lubridate
#' @import tibble
#' @import stringr
#' @import cli
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export

gta_data_slicer <- function(data = NULL, data.path = "data/master.Rds",
                            gta.evaluation = NULL, affected.flows = NULL, implementing.country = NULL,
                            keep.implementer = TRUE, affected.country = NULL, keep.affected = NULL,
                            incl.affected.strictness = "ONEPLUS", keep.others = TRUE, nr.affected = c(0, 999),
                            nr.affected.incl = "ALL", announcement.period = NULL, implementation.period = NULL,
                            keep.implementation.na = NULL, revocation.period = NULL, keep.revocation.na = TRUE,
                            submission.period = NULL, in.force.on.date = Sys.Date(), keep.in.force.on.date = "any",
                            intervention.types = NULL, keep.type = NULL, mast.chapters = NULL, keep.mast = NULL,
                            implementation.level = NULL, keep.level = NULL, eligible.firms = NULL, keep.firms = NULL,
                            cpc.sectors = NULL, keep.cpc = NULL, hs.codes = NULL, keep.hs = NULL,
                            intervention.ids = NULL, keep.interventions = NULL, lag.adjustment = NULL,
                            add.unpublished = FALSE) {
    # if master dataset is not provided, load it as data table
    # if it is already provided, ensure that it is formatted as a data.table
    if (is.null(data)) data <- data.table::as.data.table(readRDS(data.path))

    if (!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
    }

    filter_statement <- vector("character")
    # Suppress summarize info
    options(dplyr.summarise.inform = FALSE)
    ##################################################################
    # Check argument validity and generate filter statement & further operations where necessary
    ##################################################################

    # check vaildity of required / and or prepopulated function arguments
    gtalibrary::gta_logical_check(keep.implementer, is.logical, "Keep.implementer must be TRUE/FALSE")
    gtalibrary::gta_logical_check(keep.revocation.na, is.logical, "Keep.revocation.na must be a TRUE/FALSE")
    gtalibrary::gta_logical_check(keep.others, is.logical, "Keep.others must be a TRUE/FALSE")
    gtalibrary::gta_logical_check(add.unpublished, is.logical, "add.unpublished must be a TRUE/FALSE")
    gtalibrary::gta_parameter_check(tolower(nr.affected.incl), c("all", "selected", "unselected"))
    gtalibrary::gta_logical_check(in.force.on.date, lubridate::is.Date, error_msg = "in.force.on.date must be a date")
    gtalibrary::gta_parameter_check(tolower(keep.in.force.on.date), c("any", "yes", "no"))
    gtalibrary::gta_logical_check(nr.affected, \(x) (length(x) == 2 & x >= 0 & trunc(x) == x), error_msg = "nr.affected must be a vector of two valid integers >= 0")

    # gta.evaluation
    if (!is.null(gta.evaluation)) {
        gtalibrary::gta_parameter_check(tolower(gta.evaluation), c("red", "amber", "green"), arg_name = "gta.evaluation")
        gta_evaluation_filter <- stringr::str_to_title(gta.evaluation)
        filter_statement <- append(filter_statement, "gta.evaluation %in% gta_evaluation_filter")
    }

    # affected flows
    if (!is.null(affected.flows)) {
        gtalibrary::gta_parameter_check(tolower(affected.flows), c("inward", "outward", "outward subsidy"), arg_name = "affected.flows")
        affected_flow_filter <- tolower(affected.flows)
        filter_statement <- append(filter_statement, "affected.flow %in% affected_flow_filter")
    }

    # check implementing country
    if (!is.null(implementing.country)) {
        gtalibrary::gta_logical_check(keep.implementer, is.logical, "keep.implementer must be TRUE/FALSE")
        implementing_country_filter <- gtalibrary::gta_un_code_vector(countries = implementing.country)
        if (!keep.implementer) {
            filter_statement <- append(filter_statement, "!i.un %in% implementing_country_filter")
        } else {
            filter_statement <- append(filter_statement, "i.un %in% implementing_country_filter")
        }
    }

    # must be done in the end after total filtering!
    # check affected.country
    if (!is.null(affected.country)) {
        gtalibrary::gta_logical_check(keep.affected, is.logical, "keep.affected must be TRUE/FALSE")
        gtalibrary::gta_logical_check(keep.others, is.logical, "keep.others must be TRUE/FALSE")
        affected_country_filter <- gtalibrary::gta_un_code_vector(countries = affected.country)

        # get intervention ids of affected countries
        if (keep.affected) {
            filter_affected <- "a.un %in% affected_country_filter"
        } else {
            filter_affected <- "!a.un %in% affected_country_filter"
        }

        affected_interventions <- dtplyr::lazy_dt(data) |>
            dplyr::filter(eval(parse(text = filter_affected))) |>
            dplyr::pull(intervention.id) |>
            unique()

        # if any of the arguments deviate from the baseline, calculate information and adjust filtering of intervention.ids
        if (!all(nr.affected == c(0, 999)) | tolower(nr.affected.incl) != "all" | tolower(incl.affected.strictness) != "oneplus") {
            ###### ¨¨¨¨
            ###### !!! CHECK IF ADDITIONAL INFO WAS APPENDED TO THE DATA FRAME THAT COUNS THE AFFECTED TIMES!!!
            ###### ¨¨¨¨

            # generates tibble that contains
            # - number of affected countries per intervention.id where at least one member in the argument "affected.country" is included
            # - number of values "affected.country" which are affected per intervention.id
            affected_countries_info <- dtplyr::lazy_dt(data) |>
                dplyr::filter(intervention.id %in% affected_interventions) |>
                dplyr::select(a.un, intervention.id) |>
                unique() |>
                dplyr::mutate(affected_country = ifelse(a.un %in% affected_country_filter, TRUE, FALSE)) |>
                dplyr::group_by(intervention.id) |>
                dplyr::summarize(
                    n_affected_total = length(a.un), # number of different affected countries per intervention.id
                    n_affected_selected = length(a.un[affected_country]), # number of diff. affected countries per intervention.id which are in "affected.country"
                    n_affected_unselected = length(a.un[!affected_country])
                )

            # test which statement was modified and select the interventions accordingly
            if (tolower(incl.affected.strictness) != "oneplus") {
                # determine value which n_affected_selected must fulfill
                incl.affected.strictness <- switch(tlower(incl.affected.strictness),
                    "all" = length(affected_country_filter),
                    "one" = 1
                )

                affected_countries_info <- affected_countries_info |>
                    dplyr::filter(n_affected_selected == incl.affected.strictness)
            }

            if (!all(nr.affected == c(0, 999))) {
                # specifies the column of the affected_countries_info df that is evaluated
                nr.affected.incl <- switch(tolower(nr.affected.incl),
                    "all" = "n_affected_total",
                    "selected" = "n_affected_selected",
                    "unselected" = "n_affected_unselected"
                )

                # filter for the range of affected countries in the respective column
                affected_countries_info <- affected_countries_info |>
                    dplyr::filter(dplyr::between(eval(parse(text = nr.affected.incl)), nr.affected[1], nr.affected[2]))
            }


            # retrieve all the intervention.ids that fulfill the given criteria
            affected_interventions <- affected_countries_info |>
                dplyr::pull(intervention.id) |>
                unique()
        }

        # if keep.affected = TRUE, intervention.ids can be taken over, if false, additionally filter for a.un %in% affected.country
        if (keep.others) {
            filter_statement <- append(filter_statement, "intervention.id %in% affected_interventions")
        } else {
            filter_statement <- append(filter_statement, "intervention.id %in% affected_interventions & a.un %in% affected_country_filter")
        }
    }

    # intervention.types
    if (!is.null(intervention.types)) {
        gtalibrary::gta_logical_check(keep.type, is.logical, "keep.type must be TRUE/FALSE")
        permissible_values <- tolower(gtalibrary::int.mast.types$intervention.type)
        gtalibrary::gta_parameter_check(intervention.types, permissible_values)
        intervention_types_filter <- intervention.types

        if (!keep.type) {
            filter_statement <- append(filter_statement, "!intervention.type %in% intervention_types_filter")
        } else {
            filter_statement <- append(filter_statement, "intervention.type %in% intervention_types_filter")
        }
    }

    # mast.chapters
    if (!is.null(mast.chapters)) {
        gtalibrary::gta_logical_check(keep.mast, is.logical, "keep.mast must be TRUE/FALSE")
        mast_chapters_filter <- toupper(stringr::str_remove_all(pattern = "[0-9]+", string = mast.chapters))
        permissible_values <- toupper(gtalibrary::int.mast.types$mast.chapter.id)
        gtalibrary::gta_parameter_check(mast_chapters_filter, permissible_values)

        if (!keep.mast) {
            filter_statement <- append(filter_statement, "!mast.id %in% mast_chapters_filter")
        } else {
            filter_statement <- append(filter_statement, "mast.id %in% mast_chapters_filter")
        }
    }

    # eligible.firms
    if (!is.null(eligible.firms)) {
        gtalibrary::gta_logical_check(keep.firms, is.logical, "keep.firms must be TRUE/FALSE")
        permissible_values <- tolower(gtalibrary::elig.firms$eligible.firms)
        eligible_firms_filter <- tolower(eligible.firms)
        gtalibrary::gta_parameter_check(eligible_firms_filter, admissible.values, arg_name = "eligible.firms")

        if (!keep.firms) {
            filter_statement <- append(filter_statement, "eligible.firms %in% eligible_firms_filter")
        } else {
            filter_statement <- append(filter_statement, "eligible.firms %in% eligible_firms_filter")
        }
    }

    # implementation.level
    if (!is.null(implementation.level)) {
        gtalibrary::gta_logical_check(keep.level, is.logical, "kep.level must be TRUE/FALSE")
        permissible_values <- tolower(gtalibrary::imp.levels$implementation.level)
        implementation_level_filter <- tolower(implementation.level)
        gtalibrary::gta_parameter_check(implementation_level_filter, admissible.values, arg_name = "implementation.level")

        if (!keep.level) {
            filter_statement <- append(filter_statement, "!implementation.level %in% implementation_level_filter")
        } else {
            filter_statement <- append(filter_statement, "implementation.level %in% implementation_level_filter")
        }
    }

    # intervention.ids
    if (!is.null(intervention.ids)) {
        gtalibrary::gta_logical_check(keep.interventions, is.logical, "keep.interventions must be TRUE/FALSE")
        permissible_values <- unique(master$intervention.id)
        gtalibrary::gta_parameter_check(intervention.ids, permissible_values)
        intervention_ids_filter <- intervention.ids
        if (!keep.interventions) {
            filter_statement <- append(filter_statement, "!intervention.id %in% intervention_ids_filter")
        } else {
            filter_statement <- append(filter_statement, "intervention.id %in% intervention_ids_filter")
        }
    }

    # implementation.period
    if (!is.null(implementation.period)) {
        gtalibrary::gta_logical_check(implementation.period, \(x) length(x) == 2, error_msg = "Implementation.period must be a vector of 2 elements")
        gtalibrary::gta_logical_check(keep.implementation.na, is.logical, error_msg = "keep.implementation.na must be TRUE/FALSE")
        gtalibrary::gta_logical_check(implementation.period[1], lubridate::is.Date, error_msg = "implementation.period[1] must be a Date")
        gtalibrary::gta_logical_check(implementation.period[1], \(x) (lubridate::is.Date(x) | is.na(x)), error_msg = "implementation.period[2] must be either a Date or NA")

        # convert NA (--> no end date) to a data for easier filtering below
        if (is.na(implementation.period[2])) implementation.period[2] <- as.Date("9999-12-31")

        if (!keep.implementation.na) {
            filter_statement <- append(filter_statement, "!dplyr::between(date.implemented, implementation.period[1], implementation.period[2])")
        } else {
            filter_statement <- append(filter_statement, "dplyr::between(date.implemented, implementation.period[1], implementation.period[2])")
        }
    }
    # announcement.period
    if (!is.null(announcement.period)) {
        gtalibrary::gta_logical_check(announcement.period, \(x) length(x) == 2, error_msg = "announcement.period must be a vector of two elements")
        gtalibrary::gta_logical_check(announcement.period[1], lubridate::is.Date, error_msg = "announcement.period[1] must be a Date")
        gtalibrary::gta_logical_check(announcement.period[1], \(x) (lubridate::is.Date(x) | is.na(x)), error_msg = "announcement.period[2] must be either a Date or NA")

        if (is.na(announcement.period[2])) announcement.period[2] <- as.Date("9999-12-31")

        filter_statement <- append(filter_statement, "dplyr::between(date.announced, announcement.period[1], announcement.period[2])")
    }

    # submission.period
    if (!is.null(submission.period)) {
        gtalibrary::gta_logical_check(submission.period, \(x) length(x) == 2, error_msg = "submission.period must be a vector of two elements")
        gtalibrary::gta_logical_check(submission.period[1], lubridate::is.Date, error_msg = "submission.period[1] must be a Date")
        gtalibrary::gta_logical_check(submission.period[1], \(x) (lubridate::is.Date(x) | is.na(x)), error_msg = "submission.period[2] must be either a Date or NA")

        if (is.na(submission.period[2])) submission.period[2] <- as.Date("9999-12-31")

        filter_statement <- append(filter_statement, "dplyr::between(date.published, announcement.period[1], announcement.period[2])")
    }

    # revocation.period
    if (!is.null(revocation.period)) {
        gtalibrary::gta_logical_check(keep.revocation.na, is.logical, error_msg = "keep.revocation.na must be TRUE/FALSE")
        gtalibrary::gta_logical_check(revocation.period[1], lubridate::is.Date, error_msg = "revocation.period[1] must be a Date")
        gtalibrary::gta_logical_check(revocation.period[1], \(x) (lubridate::is.Date(x) | is.na(x)), error_msg = "revocation.period[2] must be either a Date or NA")
        gtalibrary::gta_logical_check(revocation.period, \(x) length(x) == 2, error_msg = "revocation.period must be a vector of two elements")

        if (is.na(revocation.period[2])) revocation.period[2] <- as.Date("9999-12-31")
        filter_statement <- append(filter_statement, "dplyr::between(date.removed, announcement.period[1], announcement.period[2])")
    }

    # in.force.on.date
    if (keep.in.force.on.date != "any") {
        keep.in.force.on.date <- tolower(keep.in.force.on.date)
        gtalibrary::gta_parameter_check(keep.in.force.on.date, c("yes", "no"))
        gtalibrary::gta_logical_check(in.force.on.date, lubridate::is.Date, error_msg = "in.force.on.date must be a date")

        if (keep.in.force.on.date == "yes") {
            filter_evaluation <- append(filter_evaluation, "date.implemented <= in.force.on.date & (is.na(date.removed) | date.removed >= in.force.on.date)")
        } else {
            filter_evaluation <- append(filter_evaluation, "date.removed < in.force.on.date & date.implemented < in.force.on.date | is.na(date.implemented)")
        }
    }

    # lag adjustment
    if (!is.null(lag.adjustment)) {
        gtalibrary::gta_logical_check(lag.adjustment, is.character, error_msg = "make sure that lag.adjustment is a character of type MM-DD")
        gtalibrary::gta_logical_check(lag.adjustment, \(x) lubridate::is.Date(as.Date(x, format = "%m-%d")))

        cutoff_date <- lubridate::ymd(paste(lubridate::year(data$date.implemented), lag.adjustment, sep = "-"))
        filter_statement <- append(filter_statement, "date.implemented <= cutoff_date")
    }

    # filter the data frame for the first time
    if (!length(filter_statement) == 0) {
        filter_statement <- paste(filter_statement, collapse = " & ")
        data <- dtplyr::lazy_dt(data) |>
            dplyr::filter(eval(parse(text = filter_statement))) |>
            data.table::as.data.table()
    }

    # hs codes
    if (!is.null(hs.codes)) {
        gtalibrary::gta_logical_check(keep.hs, is.logical, "keep.hs must be TRUE/FALSE")
        hs_codes_filter <- gtalibrary::gta_hs_code_check(codes = hs.codes, message = FALSE)
        hs_codes_filter <- stringr::str_pad(hs_codes_filter, width = 6, side = "left", pad = "0")

        if (!keep.hs) {
            filter_statement_hs <- "!affected.product %in% hs_codes_filter"
        } else {
            filter_statement_hs <- "affected.product %in% hs_codes_filter"
        }

        # keep as tibble and not as data.table --> Faster performance
        data <- tibble::as_tibble(data) |>
            dplyr::mutate(affected.product = stringr::str_split(string = affected.product, pattern = ", ")) |>
            tidyr::unnest(cols = affected.product) |>
            dplyr::filter(eval(parse(text = filter_statement_hs))) |>
            dplyr::group_by(dplyr::across(-affected.product)) |>
            dplyr::summarize(affected.product = paste(affected.product, collapse = ", "))
    }

    # cpc.sectors
    if (!is.null(cpc.sectors)) {
        gtalibrary::gta_logical_check(keep.cpc, is.logical, "keep.cpc must be TRUE/FALSE")
        cpc_sectors_filter <- gtalibrary::gta_cpc_code_check(codes = cpc.sectors)

        if (!keep.cpc) {
            filter_cpc <- "!affected.sector %in% cpc_sectors_filter"
        } else {
            filter_statement_cpc <- "affected.sector %in% cpc_sectors_filter"
        }

        data <- tibble::as_tibble(data) |>
            dplyr::mutate(affected.sector = str_split(string = affected.sector, pattern = ", ")) |>
            tidyr::unnest(cols = affected.sector) |>
            dplyr::filter(eval(parse(text = filter_cpc))) |>
            dplyr::group_by(dplyr::across(-affected.sector)) |>
            dplyr::summarize(affected.product = paste(affected.sector, collapse = ", "))
    }

    return(tibble::as_tibble(data))
}
