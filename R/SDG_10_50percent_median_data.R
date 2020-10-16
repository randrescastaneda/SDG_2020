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

yrs <- c(1993:2017)
metadata_path <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/506801/povcalnet_comparability.csv"

md  <- read_csv(metadata_path)
cr  <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")

DT <- as.data.table(dfc)
cr <- as.data.table(cr)
md  <- as.data.table(md)
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

server = NULL
dm <- pmap_df(DL, povcalnet,
           fill_gaps = TRUE,
           server = server)

dm <- as.data.table(dm)
setkeyv(dm, srtvars)

pcnvars <- c("countrycode", "year", "coveragetype", "datatype", "headcount", "povertyline")
srtvars <- c("countrycode", "year", "coveragetype", "datatype")

dm <- dm[,
         ..pcnvars
][,
  median := povertyline*2
][cr,
  on = "countrycode",
  countryname := i.countryname
][,
  text := paste("Country: ",   countrycode, "\n",
                "year: ",      year, "\n",
                "Median: ",    median, "\n",
                "Headcount: ", headcount)
]



# Fix comparability metadata
cols <- c("coveragetype", "datatype")
md[,
   (cols) := lapply(.SD, as.character),
   .SDcols = cols
][,
  coveragetype := fcase(coveragetype == "1", "R",
                        coveragetype == "2", "U",
                        coveragetype == "3", "N",
                        coveragetype == "4", "A"
  )
][,
  datatype := fifelse(datatype == "1", "consumption", "income")
]

# Merge with comparability metadata
dm[md,
   on = .(countrycode, coveragetype, datatype, year),
   compare := i.comparability
]


# add text variable
dm <- dm[!is.na(compare)
][,
  mcomp := compare == max(compare),
  by = .(countryname, datatype, coveragetype)
][,
  ggtext := fifelse(year == max(year) | year == min(year),
                    paste0(countryname, "\n", year), NA_character_
  ),
  by = .(countryname, datatype, coveragetype)
]


dm <- unique(dm)


# # --------- Save data ---------
readr::write_rds(dm, here("data", "SDG_10_50percent_median.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Regional and global level   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



