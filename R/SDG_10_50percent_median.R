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
library("povcalnet")

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

yrs <- c(1993, 2002, 2010, 2015, 2017)
rs <- c(1:5)

DT <- DT[goal == 50 & year  %in% yrs]

oldn <- c("countrycode", "pv")
newn <- c("country", "povline")
setnames(DT, oldn, newn, skip_absent = TRUE)


DF <- DT[,
   c("country", "povline", "year")
  ][
    ,
    povline := povline/2
  ]

DF <- as.list(DF)

# dm <- pmap_dfr(DF, povcalnet,
#            fill_gaps = TRUE,
#            server = "AR")

#--------- Save data ---------
# write_rds(dm, here("data", "SDG_10_50percent_median.rds"))

#----------------------------------------------------------
# compare to international poverty
#----------------------------------------------------------

# load headcount of 50 percent the median

pcnvars <- c("countrycode", "year", "coveragetype", "datatype", "headcount")
srtvars <- c("countrycode", "year", "coveragetype", "datatype")

dm <- read_rds(here("data", "SDG_10_50percent_median.rds"))
dm <- as.data.table(dm)
setkeyv(dm, srtvars)

dm <- dm[
        year  %in% yrs
        ][
          ,
          ..pcnvars
        ]

setnames(dm, "headcount", "med_2")

dm <- unique(dm)

# Headcount at international lines
de <- povcalnet(fill_gaps = TRUE,
                server    = "AR")

df <- as.data.table(de)
setkeyv(df, srtvars)

df <- df[year  %in% yrs
   ][
     ,
     ..pcnvars
   ]

setnames(df, "headcount", "pov")
df <- unique(df)


#--------- merge datasets ---------
df <- df[dm] # no need `on` because we are using key


#--------- prepare data ---------
df <- df[
  # Exclude poverty == 0
  pov > 0
  ][
  ,
  c("pov_g", "med_g", "text") := {

    pov_g = ann_growth(pov, year)
    med_g = ann_growth(med_2, year)
    text = paste0("Country: ", countrycode, "\n",
                  "Abs pov: ", round(pov_g, digits = 2), "\n",
                  "Rel pov: ", round(med_g, digit = 2), "\n",
                  "Year 2: " , year, "\n")
    list(pov_g, med_g, text)
    },
  by = .(countrycode, coveragetype)
  ][
    pov_g > 0 & med_g > 0
  ]



pt <- ggplot(df[pov_g < max(pov_g, na.rm = TRUE)],
       aes(
        x = pov_g,
        y = med_g,
        text = text
       )
       ) +
  geom_point()

plotly::ggplotly(pt, tooltip = "text")


#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

