# ==================================================
# project:       Global poverty projections taking into account COVID.
#                 Based on the blog and Lakner's et al paper
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-08-26
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             dataframe and chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("data.table")
library("here")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#   load and clean data
#----------------------------------------------------------
# load data
DT <- haven::read_dta(here("data", "Projections_constant2019.dta"))
setDT(DT)

# clean data

DT[,
   growth := fifelse(
                grepl("^(?!.*target)", growth, perl = TRUE),
                gsub("^g[\\d]+", "",   growth, perl = TRUE),
                "target"
              )
   ]

# columns to keep
cols <- c("growth", "alpha", "year", "pop")
cols <- c(cols, grep("FGT", names(DT), value = TRUE))

# name columsn for  No. of poor
fgtcols <- grep("FGT", names(DT), value = TRUE)
npcols  <- paste("poor", gsub("FGT0_", "", fgtcols), sep = "")

DT <- DT[ # Keep only relevant observations
          growth !=  "target"
         & gic          %chin% c("l", "")
         & passthrough  %chin% c("glob", "")
         ][
           ,
           # Keep only relevant variables
           ..cols
           ][
             ,
             # Convert FGT from 0-100  to decimals
             (fgtcols) := lapply(.SD, function(x) {x/100}),
             .SDcols = fgtcols
           ][
             ,
             #Convert population to millions
             pop := pop*1e6
             ][
               ,
               # Calculate Number of poor
               (npcols) := lapply(.SD, function(x) {x*pop}),
               .SDcols = fgtcols
             ]



# Aggregates
DA <- DT[,
         lapply(.SD, weighted.mean, w = pop),
         .SD = patterns("FGT"),
         by = .(year, growth, alpha)
         ]


DB <- DT[,
         lapply(.SD, sum),
         .SD = patterns("poor"),
         by = .(year, growth, alpha)
         ]

iname <- paste("i.", npcols, sep = "")

DA[DB,
   on = .(year, growth, alpha),
   (npcols) := mget(iname)
   ]
rm(DB)



#----------------------------------------------------------
#
#----------------------------------------------------------
