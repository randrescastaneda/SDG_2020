# ==================================================
# project:       iterate through povcalnet_interate
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-25
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             dataframe
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#
#----------------------------------------------------------

source("R/povcalnet_iterate.R")


year <-
  povcalnet(country = "COL",
            fill_gaps = TRUE) %>%
  select(year) %>% pull

year <- c(1981, 1990, 1999, 2005, 2010, 2015)

regions  <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP", "WLD")

# countries <- map_dfr(regions,  function(x)
#   data.frame(country = get_countries(region_code = x))) %>%
#   pull


#------ regions
rdf <- as.list(expand.grid(goal = c(c(1:9) / 10, .99),
                           region = regions,
                           year = year,
                           stringsAsFactors = FALSE))

dfr <- pmap_dfr(rdf, povcalnet_iterate)
save(dfr, file = "data/dfr.RData")


#------- countries
countries <- get_countries("WLD")
cdf <- as.list(expand.grid(goal = c(c(1:9) / 10, .99),
                           country = countries,
                           year = year,
                           tolerance = 4,
                           stringsAsFactors = FALSE))



dfc <- pmap_dfr(cdf, povcalnet_iterate)
save(dfc, file = "data/dfc.RData")
