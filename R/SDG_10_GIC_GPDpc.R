# ==================================================
# project:       Growth Incidence Curve of GDP percapita
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-10-15
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

library(WDI)
library(readr)
library(dplyr)
library(ggplot2)
library("data.table")
library("janitor")
library("here")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
qtile <- function(x, nq = 10) {
  N   <-  length(x)
  csw <-  1:N
  qp  <-   floor(csw/((N+1)/nq)) + 1
  return(qp)
}


#----------------------------------------------------------
#   Set up
#----------------------------------------------------------

# WDIsearch('gdp.*capita.*constant')
codes <- read_rds("data/cty_regs_names.rds")

iso <- WDI_data$country %>%
  as_tibble() %>%
  select(countrycode = iso3c,iso2c) %>%
  inner_join(codes, by = "countrycode")


gdp <-  WDI(indicator = "NY.GDP.PCAP.KD",
           start       = 1993,
           end         = 2017) %>%
  inner_join(iso, by = "iso2c") %>%
  rename(gdp = NY.GDP.PCAP.KD)


#----------------------------------------------------------
#
#----------------------------------------------------------


# set data.table
input <- list(

  yr1 = 1993,
  yr2 = 2017,
  nq  = 10,
  pc  = 50,
  ms  = "max"
)


GDP <- as.data.table(gdp)

GDP <- GDP[
  # remove old years
  year >= 1990
][
  # filter negative values (which we should not have)
  gdp > 0 & !is.na(gdp)
]

#----------------------------------------------------------
# prepare database
#----------------------------------------------------------

calc <- paste0(".(", input$ms, "  = ", input$ms, "(gdp, na.rm = TRUE))")
calc <- parse(text = calc)


# Sort

DQ <-
  GDP[
    year  %in% c(input$yr1, input$yr2)
  ]

setorder(DQ, year, gdp)

DA <- copy(DQ)

#--------- anonymous ---------

DA <-
  DA[
    ,# Create deciles in each percentile
    qp := qtile(gdp, input$nq),
    by = .(year)
  ][
    , # Make requested calculation
    eval(calc),
    by = .(year, qp)
  ][
    ,
    yr := ifelse(year == input$yr1, "yr1", "yr2")
  ]


DA <- dcast(DA,
            formula = qp ~ yr,
            value.var = input$ms)

DA[
  ,
  gic := ((yr2/yr1)^(1 / (input$yr2 - input$yr1))) - 1
]


#--------- Non-Anonymous ---------

QP <- DQ[year == input$yr1
][,
  qp := qtile(gdp, input$nq)
][,
  c("countrycode", "qp")
]

DN <- DQ[QP,
         on = .(countrycode)
][
  , # Make requested calculation
  eval(calc),
  by = .(year, qp)
][
  ,
  yr := ifelse(year == input$yr1, "yr1", "yr2")
]

DN <- dcast(DN,
            formula = qp ~ yr,
            value.var = input$ms)

DN[
  ,
  gic := ((yr2/yr1)^(1 / (input$yr2 - input$yr1))) - 1
]




ggplot(DA,
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




ggplot(DN,
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





#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------





