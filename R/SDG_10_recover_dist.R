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
source("R/utils.R")

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")

years <- c(1993,2002, 2015)
cts <- unique(cr$countrycode)

cty_yr <- expand.grid(year = years,
                      country = cts,
                      coverage = "national",
                      stringsAsFactors = FALSE) %>%
  mutate(
    coverage = if_else(country == "ARG", "urban", coverage)
  ) %>%
  filter(!(country  %in% c("IND", "IDN", "CHN") )) %>%
  bind_rows(
      expand.grid(year = years,
                  country = c("IND", "IDN", "CHN"),
                  coverage = c("urban", "rural"),
                  stringsAsFactors = FALSE)
  ) %>%
  as.list()


# cty_yr <- list(
#    country  = c("XKX", "XKX"),
#    year     =  c(1993, 2002),
#    # coverage =  c("urban", "rural")
#    coverage =  c("national", "national")
#  )


# rvd_dists <- pmap(cty_yr, rcv_dist, step = 10, pl = 1)
rvd_dists <- pmap(cty_yr, rcv_dist, step = .5, pl = .5)

names(rvd_dists) <- as_tibble(cty_yr) %>%
  transmute(paste0(country,year, coverage))  %>%
  pull()


# str(rvd_dists)

write_rds(rvd_dists, "data/recovered_dist.rds")




# a <- rcv_dist(country = "XKX", year = 1993,coverage = "national", step = 20, pl = 1)
