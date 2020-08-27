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
#   load and clean data
#----------------------------------------------------------
# load data
DT <- haven::read_dta(here("data", "Projections_constant2019_int.dta"))
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

# replace missing values
DA[
   ,
   `:=`(
      growth = fifelse(growth == "", "baseline", growth),
      alpha  = fifelse(is.na(alpha), 0, alpha)
   )
   ]

# Keep best and worst scenario based on inequality projection
DA <- DA[
   (alpha == 0 & year <= 2019)
   | (alpha %in% c(-2,2) & year >= 2019)
   ][,
     scenario := fcase(
        alpha == -2, "G",
        alpha ==  2, "B",
        default = "D")
   ][,
     alpha := NULL
     ]

# reshape to long poverty line
DC <- melt(DA,
           id              = c("year", "growth", "scenario"),
           measure.vars    = patterns(fgt   = "FGT0_",
                                      npoor = "poor"),
           variable.name   = "povline",
           variable.factor = FALSE
           )

DC[,
   povline := fcase(
                povline == "1", "1.9",
                povline == "2", "3.2",
                povline == "3", "5.5"
         )
   ]

# Reshape wide on scanario for plotting area
DB <- dcast(DC,
            year + growth + povline ~ scenario,
            value.var = c("fgt", "npoor")
            )

