# ==================================================
# project:       recover countries distribution
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-02
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             different data frames per country
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("povcalnetR")
library("progress")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/povcalnet_iterate.R")

rcv_dist <- function(country, year, qtl = 500) {

  print(paste("workging on", cty, year))
  tryCatch(
    expr = {
      # Your code...
      max_th <- povcalnet_iterate(country = country,
                                  year = year,
                                  goal = .999,
                                  tolerance = 4) %>%
        select(threshold) %>%
        pull()

      step <- round(max_th/qtl, digits = 2)

      pls <- seq(from = 0.01,
                 to = max_th,
                 by = step)

      pb <- progress_bar$new(total = length(pls))
      povcalcall <- function(pl, country, year) {
        pb$tick()
        df <- povcalnet(country = country, povline = pl, year = year)
        return(df)
      }

      cty_data <- map_df(pls, povcalcall, country = country, year = year)


      return(cty_data)
    }, # end of expr section

    error = function(e) {
      return(e$message)
    } # end of error section

  ) # End of trycatch

}

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")

year <- c(1993,2002, 2015)
cts <- unique(cr$countrycode)

cty_yr <- as.list(
  expand.grid(year = year,
  country = cts)
  )


cty_yr <- list(
  country  = c("ARG", "ARG", "COL", "COL"),
  year     =  c(2002, 2015, 2002, 2015)
)


rvd_dists <- pmap(cty_yr, rcv_dist, qtl = 10)

names(rvd_dists) <- as_tibble(cty_yr) %>%
  transmute(paste0(country,year))  %>%
  pull()

rvd_dists
