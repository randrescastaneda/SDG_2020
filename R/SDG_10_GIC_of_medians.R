# ==================================================
# project:       GIC of medians
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-07-21
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             temporal code for shiny app
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

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


qtile <- function(x, nq = 10) {
  N   <-  length(x)
  csw <-  1:N
  qp  <-   floor(csw/((N+1)/nq)) + 1
  return(qp)
}


#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
dfc <- read_rds("data/dfc.rds")


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

# set data.table
input <- list(

yr1 = 1993,
yr2 = 2017,
nq  = 100,
pc  = 50,
ms  = "max"
)


DT <- as.data.table(dfc)
setnames(DT, "threshold", "pv")


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
]

#----------------------------------------------------------
# prepare database
#----------------------------------------------------------

calc <- paste0(".(", input$ms, "  = ", input$ms, "(pv, na.rm = TRUE))")
calc <- parse(text = calc)


# Sort


DQ <-
  DT[
    year  %in% c(input$yr1, input$yr2)
    & goal == input$pc
    ]

setorder(DQ, year, pv)
DQ <-
  DQ[
    ,# Create deciles in each percentile
    qp := qtile(pv, input$nq),
    by = .(year)
    ][
      , # Make requested calculation
      eval(calc),
      by = .(year, qp)
    ][
      ,
      yr := ifelse(year == input$yr1, "yr1", "yr2")
    ]


DQ <- dcast(DQ,
            formula = qp ~ yr,
            value.var = input$ms)

DQ[
  ,
  gic := ((yr2/yr1)^(1 / (input$yr2 - input$yr1))) - 1
  ]


ggplot(DQ,
       aes(
         x = qp,
         y = gic
       )
      ) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0,
             color = "red") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Growth Incidence curve of selected percentile",
    x     = "Quantiles",
    y     = "Annualized growth"
  )


