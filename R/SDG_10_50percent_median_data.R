# ==================================================
# project:       50 percent of median
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-07-14
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("here")
library("povcalnetR")
library("plotly")
library("ggrepel")



#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

ann_growth <- function(x,y) {
  x0 <- shift(x)
  y0 <- shift(y)

  z <- ((x/x0)^(1/(y-y0))) - 1
}



#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
yrs <- c(1993, 2002, 2010, 2015, 2017)
yrs <- c(1993:2017)
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")

DT <- as.data.table(dfc)
cr <- as.data.table(cr)
setnames(DT, "threshold", "pv")

# Sort
setorder(DT, year, goal, pv)

DT <- DT[
  # remove old years
  year >= 1990
][
  # filter negative values (which we should not have)
  pv > 0 & !is.na(pv)
][,
  # multiply by 100
  goal := 100*goal

][
  ,
  headcount := NULL
][
  # Merge country names and regions
  cr,
  on = .(countrycode)
]



#----------------------------------------------------------
#   Get medians
#----------------------------------------------------------

DT <- DT[goal == 50 & year  %in% yrs]

oldn <- c("countrycode", "pv")
newn <- c("country", "povline")
setnames(DT, oldn, newn, skip_absent = TRUE)

DT2 <- copy(DT)

DF <-
  DT2[
    ,
    coverage := ifelse(coverage == "U", "urban",
                       ifelse(coverage == "R", "rural", "national")
                       )
  ][,
     c("country", "povline", "year", "coverage")
    ][
      ,
      povline := povline/2
    ]

rm(DT2)
#
DL <- as.list(DF)

dm <- pmap_df(DL, povcalnet,
           fill_gaps = TRUE,
           server = "int")

# # --------- Save data ---------
write_rds(dm, here("data", "SDG_10_50percent_median.rds"))
