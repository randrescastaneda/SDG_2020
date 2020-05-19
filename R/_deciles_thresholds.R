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
wdir <- "C:/Users/wb384996/OneDrive - WBG/WorldBank/DECDG/SDG_2020"
if (getwd() != wdir) {
  setwd(wdir)
}


#----------------------------------------------------------
#
#----------------------------------------------------------

source("R/povcalnet_iterate.R")


# year <-
#   povcalnet(country = "COL",
#             fill_gaps = TRUE) %>%
#   select(year) %>% pull

# year <- c(1981, 1990, 1999, 2005, 2010, 2015)
year <- c(1993, 2002, 2015)


# countries <- map_dfr(regions,  function(x)
#   data.frame(country = get_countries(region_code = x))) %>%
#   pull

#------- countries
countries <- get_countries("WLD")
cdf <- as.list(expand.grid(goal = c(c(1:9) / 10, .9999),
                           country = countries,
                           year = year,
                           tolerance = 4,
                           stringsAsFactors = FALSE))



dfc <- pmap_dfr(cdf, povcalnet_iterate)
write_rds(dfc, "data/dfc.rds")

#------ regions
regions  <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP", "WLD")
rdf <- as.list(expand.grid(goal = c(c(1:9) / 10, .9999),
                           region = regions,
                           year = year,
                           stringsAsFactors = FALSE))

dfr <- pmap_dfr(rdf, povcalnet_iterate)
write_rds(dfr, "data/dfr.rds")




