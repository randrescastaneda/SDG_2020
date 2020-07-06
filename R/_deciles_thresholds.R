# ==================================================
# project:       recover distribution using popshare
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

wran_povcal <- function(popshare, wb = FALSE, ...) {

  if(wb == FALSE) {
    df <- povcalnet(popshare = popshare, ...)
    ctr <- "countrycode"
  } else {
    df <- povcalnet_wb(popshare = popshare, ...)
    ctr <- "regioncode"
  }


  df <- df %>%
    mutate(
      goal = popshare
    ) %>%
    select(!!ctr, year,
           threshold = povertyline,
           goal, headcount)


  return(df)
}

#----------------------------------------------------------
#
#----------------------------------------------------------

#------- countries
th <- c(1:10) / 10
dfc <- map_dfr(th, ~wran_povcal(popshare = .x,
                               fill_gaps = TRUE, # Line up years
                               server = "AR")
               )

write_rds(dfc, "data/dfc.rds")

#------ regions

dfr <- map_dfr(th, ~wran_povcal(popshare = .x,
                                wb = TRUE,
                                server = "AR")
            )

write_rds(dfr, "data/dfr.rds")




