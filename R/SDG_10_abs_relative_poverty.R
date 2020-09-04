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

cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")

DT <- as.data.table(dfc)
cr <- as.data.table(cr)

pcnvars <- c("countrycode", "year", "coveragetype", "datatype", "headcount", "povertyline")
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

dm <- unique(dm)


# Headcount at international lines
de <- povcalnet(fill_gaps = TRUE,
                server    = "int")

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
setnames(dm, "headcount", "med_2")
df <- df[dm] # no need `on` because we are using key


#--------- prepare data ---------
df <- df[
  # Exclude poverty == 0
  pov > 0
][
  ,
  c("pov_g", "med_g", "text", "group") := {
    pov_g <- ann_growth(pov, year)
    med_g <- ann_growth(med_2, year)
    text  <- paste0(
      "Country: ", countrycode, "\n",
      "Abs pov: ", round(pov_g, digits = 2), "\n",
      "Rel pov: ", round(med_g, digit = 2), "\n",
      "Period: ", shift(year),"-", year, "\n"
    )
    group <- ifelse(pov_g > 0 & med_g > 0, 1,
                    ifelse(pov_g < 0 & med_g > 0, 2,
                           ifelse(pov_g < 0 & med_g < 0, 3,4)
                    )
    )
    list(pov_g, med_g, text, group)
  },
  by = .(countrycode, coveragetype)
]


pt <- ggplot(
  df[pov_g < max(pov_g, na.rm = TRUE)],
  aes(
    x = pov_g,
    y = med_g
  )
) +
  geom_point(
    aes(
      text = text,
      color = factor(group)
    )
  ) +
  geom_smooth() +
  labs(
    title = "Relative Vs Absolute poverty",
    x = "Annualized growth of Absolute poverty",
    y = "Annualized growth of Relative poverty (50% of P50)"
  ) +
  theme_minimal()

ptp <- plotly::ggplotly(pt, tooltip = "text")
ptp

plotly::api_create(ptp)
