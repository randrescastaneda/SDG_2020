# ==================================================
# project:       Sotry of hundred people
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-05-19
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts and data
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")
dfr <- read_rds("data/dfr.rds")
# lc <- read_rds("data/cts_dist.rds")

c1 <- read_rds("data/cts_dist.rds")$CRI2015national
c1$countrycode <- "CRI"

c2 <- read_rds("data/cts_dist.rds")$FIN2015national
c2$countrycode <- "FIN"

cf <- bind_rows(c2, c1)
rm(c2, c1)

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------


#----------------------------------------------------------
# charts
#----------------------------------------------------------

ggplot(data = filter(cf, welfare < 200),
       aes(x = welfare,
           weight = weight,
           color = countrycode)) +
  geom_histogram(bins = 100,
                 fill = "white",
                 position="dodge",
                 alpha = 0.5) +
  theme_classic()




#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

