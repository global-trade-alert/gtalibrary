## Roxygen documentation
#
##' GTA intra-year duration calculator
##'
##' Computes the share of each calendar year that each intervention was in force.
##'
##' @param data.path Specifies where the GTA data file is located (Default: 'data/master_plus.Rdata'). Set to 'online' to download the latest copy.
##' @param is.data.frame Specify if the data path is a data frame in the current environment. Default is FALSE. If you supply a data frame, the first three columns need to be (1) intervention ID, (2) implementation date, (3) removal date. In that order.
##' @param years The calendar years for which to calculate the shares. Calculation includes interventions based on enforcement status, not implementation date i.e. if you start in 2010, this function will also work with interventions implemneted in 2009 but still in force in 2010. Specify as c(start.year, end.year). Default is c(2008,CURRENT.YEAR).
##' @param current.year.todate Should the duration for the current year, if included, be calculated as 'share of year to date' (TRUE) or 'share of entire current year' (FALSE). Default is TRUE.
##' @param df.name Set the name of the generated result data frame. Default is intervention.duration.
##' @param pc.name Set the name of the generated parameter choice data frame. Default is parameter.choice.duration.
##' @import data.table
##' @return Output is two data frames. First data frame includes the share of each year that in intervention was in force. Second data frame states parameter choices.
##' @references www.globaltradealert.org
##' @author Global Trade Alert
#
## Function infos and parameters  --------------------------------------------
##' @export
#gta_intervention_duration <- function(data, 
#    data.path = "data/master_plus.Rdata",
#    is.data.frame = FALSE, years = NULL,
#    current.year.todate = TRUE) {
#  ## initialising
# 
# ########### IMPORT DATA HERE
#  
#  ## check years
#  if (is.null(years)) {
#    year.start <- 2008
#    year.end <- year(Sys.Date())
#  } else {
#    current_year <- lubridate::year(Sys.Date())
#    gtalibrary::gta_logical_check(
#      years,
#      \(x) (length(x) == 2 & trunc(x) == x & between(years[1], 2008, current_year) & between(years[2], 2008, current_year)), 
#      "Years must be a vector of two numeric values between 2008 and {current_year}"
#    )
#int <- lubridate::interval("2007-03-01T13:00:00Z/2008-05-11T15:30:00Z")
#
#lubridate::int_overlaps()
#calculate_year_share <- function(start, end){
#    share_start = as.numeric((start - lubridate::floor_date(start, "year"))) / 365
#    share_end = as.numeric((end - lubridate::floor_date(end, "year"))) / 365
#    years = generateSequence(lubridate::year(start), lubridate::year(end))
#}
#unlist(years)
#
#
###### make more efficient with c++ function !!!
#
#test <- tibble::tibble(start = c(Sys.Date() - 1000, Sys.Date() + 2000), end = start + 10000)
#
## do the test with a tibble again !!! 
#a <- a |>
#  drop_na(date.implemented, date.removed)
#dtplyr::lazy_dt(a) |>
#  mutate(
#    share_start = as.numeric((date.implemented - lubridate::floor_date(date.implemented, "year"))) / 365,
#    share_end = as.numeric((date.removed - lubridate::floor_date(date.removed, "year"))) / 365,
#    years = generateSequence(lubridate::year(date.implemented), lubridate::year(date.removed)),
#    share = 1
#  )
#  
#   |>
#  unnest(years) |>
#    group_by(start, end) |>
#    mutate(
#      share[1] = start_date, share[length(share)] = end_date)
#head(a)
#left_join
#int <- interval(ymd("2001-01-01"), ymd("2025-01-01"))
#
#(Sys.Date()) %within% (int)
#test |>
#  dplyr::mutate(years = dplyr::between(start,1, 2))
#
#
#microbenchmark::microbenchmark(
#  times = 1, 
#  out <- seq_vec(from = x_1, to = x_2, by = 1)
#)
#
#  dtplyr::lazy_dt(data) |>
#    tidyr::replace_na() |>
#    mutate(
#      start_year = data.table::year(implementation.date),
#      end_year = data.table::year(revocation.period), 
#      year_in_force = seq(from = start_year, to = end_year, by = 1), 
#      share_start_year = as.numeric(implementation.date - lubridate::floor_date(implementation.date, "year")) / 365, 
#      share_end_year = as.numric(revocation.period - lubridate::floor:(revocation.period, "year")) / 365
#    ) # cannot be performed on lazy data frame
#  a <- difftime(Sys.Date(), Sys.Date() - 10)
#
#start <- as.Date("2013-05-13")
#end <- as.Date("2015-03-26")
#lubridate::date_decimal(Sys.Date())
#as.numeric(Sys.Date()) ## number of days since 19408
#
#Sys.Date() - 19408
#lubridate::period(Sys.Date() - Sys.Date()-1)
#seq(start, end, "year")
#
#(start - lubridate::floor_date(start, "year")) / 365
#
#lubridate::leap_year(2008)
#
#as.numeric(a)
#master <- unique(master[, c("intervention.id", "date.implemented", "date.removed")])
#  master <- subset(master, year(date.implemented) <= year.end)
#  master$date.removed[is.na(master$date.removed)] <- as.Date(paste(year(Sys.Date()) + 1, "-01-01", sep = ""), "%Y-%m-%d")
#
#  yr.length <- data.frame(year = c(year.start:year.end), days = 365)
#  yr.length$days[yr.length$year %in% seq(2008, year.end, 4)] <- 366
#
#  duration <- data.frame(expand.grid(intervention.id = unique(master$intervention.id), year = c(year.start:year.end)), share = NA)
#  duration <- merge(duration, master, by = "intervention.id", all.x = T)
#
#  # duration is zero
#  duration$share[year(duration$date.implemented) > duration$year] <- 0
#  duration$share[year(duration$date.removed) < duration$year] <- 0
#
#  # duration is one
#  duration$share[year(duration$date.implemented) < duration$year & year(duration$date.removed) > duration$year] <- 1
#
#  # durations for cases that start/end within the given year
#  intra.year <- subset(duration, is.na(share))
#
#  if (nrow(intra.year) > 0) {
#    for (i in 1:nrow(intra.year)) {
#      if (intra.year$year[i] < year(Sys.Date())) {
#        intra.year$share[i] <- sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep = ""), "%Y-%m-%d"):as.Date(paste(intra.year$year[i], "-12-31", sep = ""), "%Y-%m-%d")))) / yr.length$days[yr.length$year == intra.year$year[i]]
#      } else {
#        ## correcting current year duration (if required)
#        if (current.year.todate) {
#          intra.year$share[i] <- sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep = ""), "%Y-%m-%d"):Sys.Date()))) / (as.numeric(Sys.Date() - as.Date(paste(year(Sys.Date()), "-01-01", sep = ""))) + 1)
#        } else {
#          intra.year$share[i] <- sum(as.numeric(c(intra.year$date.implemented[i]:intra.year$date.removed[i]) %in% c(as.Date(paste(intra.year$year[i], "-01-01", sep = ""), "%Y-%m-%d"):as.Date(paste(intra.year$year[i], "-12-31", sep = ""), "%Y-%m-%d")))) / yr.length$days[yr.length$year == intra.year$year[i]]
#        }
#      }
#    }
#    duration <- rbind(subset(duration, is.na(share) == F), intra.year)
#  }
#  duration <- unique(duration[, c("intervention.id", "year", "share")])
#
#}
#
#
#gtalibrary::gta_intervention_duration()
#
#intervention.duration |>
#  tibble::as_tibble() |>
#  dplyr::filter(share > 0) |> 
#  dplyr::arrange(share)
#
#
## A tibble: 280,317 × 3
#   intervention.id  year   share
#             <int> <int>   <dbl>
# 1            9130  2016 0.00273
# 2            9417  2008 0.00273
# 3            9418  2008 0.00273
# 4            9463  2012 0.00273
#
# library(Rcpp)
#
#sourceCpp("C:/Users/sveng/OneDrive/Dokumente/GitHub/GTA/gtalibrary/cpp_files/functions.cpp")
#
#date_getter()
#microbenchmark::microbenchmark(
#  times = 1,
#  datefunction(start, end)
#)
#
#start = rep(Sys.Date(), 10^5)
#end = start + 1000
#
#object.size(a) / 10^6
#
#gtalibrary::gta_setwd(("H"))
#
#gtalibrary::gta_intervention_duration(data.path = "master", is.data.frame = TRUE)
#
#master = test
#ncol(test)
#a$share_of_year
#tibble::as_tibble(intervention.duration)
#gtalibrary::gta_setwd("H")
#a <- readRDS("data/master.rds")
#e <- readRDS("data/master.rds")
## do this, then perform the RCPP function 
#microbenchmark::microbenchmark(
#  times = 10, 
#    {
#      test <- dtplyr::lazy_dt(a) |>
#        dplyr::select(intervention.id, date.implemented, date.removed) |>
#        dplyr::filter((date.implemented < date.removed) | is.na(date.removed)) |>
#        unique() |>
#        tibble::as_tibble() |>
#        tidyr::drop_na(date.implemented) |>
#        tidyr::replace_na(list(date.removed = Sys.Date())) |>
#        dplyr::filter(date.implemented < date.removed)
#
#      b <- datefunction(start = test$date.implemented, end = test$date.removed, current_date = Sys.Date())
#      test <- dtplyr::lazy_dt(test) |>
#        dplyr::mutate(
#          year_in_force = b$year_in_force,
#          share_of_year = b$share_of_year
#        ) |>
#        tibble::as_tibble() |>
#        tidyr::unnest(cols = c(year_in_force, share_of_year))
#    }
#)
#
#library(Rcpp)
#getwd()
#Rcpp.package.skeleton("tet", cpp_files = "src/functions.cpp")
#intervention.duration |>
#  tibble::as_tibble() |>
#  unique() |>
#  dplyr::filter(year == 2023)
#
#a |>
#  dplyr::filter(intervention.id == 5688)
#
#nrow(a)
#a
#b <- datefunction_furtheroptimized(start = a$date.implemented, end = a$date.removed)
#
#a$year_active <- b$year_in_force
#a$year_in_force <- b$share_of_year
#
#a |>
#  tidyr::unnest(cols = c(year_active, year_in_force))
#head(a)
#microbenchmark::microbenchmark(
#  times = 1, 
#  rcpp_sum(c(1:10^6)), 
#  sum(c(1:10^6))
#)
#
#test <- as.data.frame(a)
