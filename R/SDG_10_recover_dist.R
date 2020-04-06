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

year <- c(1993,2002, 2015)
cts <- unique(cr$countrycode)

cty_yr <- as.list(
  expand.grid(
    year = year,
    country = cts,
    stringsAsFactors = FALSE
  )
)


# cty_yr <- list(
#    country  = c("ARG", "COL"),
#    year     =  c(2015, 2015)
#  )


# rvd_dists <- pmap(cty_yr, rcv_dist, step = 10, pl = 1)
rvd_dists <- pmap(cty_yr, rcv_dist, step = .5, pl = .5)

names(rvd_dists) <- as_tibble(cty_yr) %>%
  transmute(paste0(country,year))  %>%
  pull()


str(rvd_dists)

write_rds(rvd_dists, "data/recovered_dist.rds")


