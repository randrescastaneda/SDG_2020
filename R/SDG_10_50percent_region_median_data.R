# ==================================================
# project:       Get the median of each region in reference years
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-09-09
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             output
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("here")
library("povcalnetR")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

source(here("R","povcalnet_iterate.R"))

filter_wb <- function(region, povline, year, server) {

  tryCatch(
    expr = {
      # Your code...
      df <- povcalnet_wb(povline = povline,
                         year    = year,
                         server  = server)
      df <- df %>%
        filter(regioncode == region) %>%
        select(regioncode, year, povertyline, headcount) %>%
        mutate(status = "OK")
    }, # end of expr section

    error = function(e) {
      df  <- tibble::tibble(
        regioncode  = region,
        year        = year,
        povertyline = povline,
        headcount   = NA,
        status      = paste("Error:",e$message)
      )
    }, # end of warning section

    finally = {
      print(paste("done with", region, year))
    } # end of finally section

  ) # End of trycatch

  return(df)
}



#----------------------------------------------------------
#   Set up
#----------------------------------------------------------

wb_regions <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP", "WLD")
years      <- c(1993, seq(1995, 2015, 5), 2017)
server     <- "int"
goal       <- .5

opts       <- expand_grid(year   = years,
                          region = wb_regions,
                          goal   = goal,
                          server = server)

opts       <- as.list(opts)

#----------------------------------------------------------
# get values
#----------------------------------------------------------

regional_medians <- pmap_dfr(opts, povcalnet_iterate)

regional_medians <-
  regional_medians %>%
  transmute(region  = countrycode,
            median  = threshold,
            year    = year,
            povline = median/2
            )

write_rds(regional_medians, "data/regional_medians.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   get pop share below 50% of median   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df <- read_rds("data/regional_medians.rds")

opts <- df %>%
  select(-median) %>%
  as.list()

popshare <-  pmap_dfr(opts, filter_wb, server = "int")

write_rds(popshare, "data/popshare_50percent_regional_median.rds")




## for testing
# wb_regions <- c("LAC")
# years      <- c(1993, 2017)
# server     <- "int"
# goal       <- .5
#
