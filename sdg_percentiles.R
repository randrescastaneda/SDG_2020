# ==================================================
# project:       b10 T90 and median for all countries
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-02
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("../dwd/povcalnet_iterate.R")
source("c:/users/wb384996/OneDrive - WBG/WorldBank/DECDG/dwd/povcalnet_iterate.R")

#----------------------------------------------------------
#
#----------------------------------------------------------

year <-
  povcalnet(country = "COL",
            fill_gaps = TRUE) %>%
  select(year) %>% pull

year <- c(1981, 1990, 1999, 2005, 2010, 2015)
#year <- c(1981, 2015)

regions  <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP", "WLD")
#regions  <- c("LAC")

# countries <- map_dfr(regions,  function(x)
#   data.frame(country = get_countries(region_code = x))) %>%
#   pull


#------ regions
rdf <- as.list(expand.grid(goal = c(.1, .5, .9),
                           region = regions,
                           year = year,
                           stringsAsFactors = FALSE))

dfr <- pmap_dfr(rdf, povcalnet_iterate)
save(dfr, file = "data/dfr.RData")


#------- countries
countries <- get_countries("WLD")
#countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "SLV", "HND", "MEX")
#countries <- c("ARG",  "BRA", "COL")

cdf <- as.list(expand.grid(goal = c(.1, .5, .9),
                           country = countries,
                           year = year,
                           tolerance = 4,
                           stringsAsFactors = FALSE))



dfc <- pmap_dfr(cdf, povcalnet_iterate)
save(dfc, file = "data/dfc.RData")


