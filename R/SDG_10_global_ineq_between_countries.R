# ==================================================
# project:       evoluation of inequality between countries
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-05-05
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

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")
dfr <- read_rds("data/dfr.rds")


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

nq <- 10
dfq <- dfc %>%
  filter(status == "OK") %>%
  pivot_wider(
    values_from = threshold,
    names_from = goal,
    names_prefix = "p",
    id_cols = c(countrycode, year)
  ) %>%
  group_by(year) %>%
  arrange(year, p50) %>%
  mutate(
    w  = 1,
    N  = sum(w, na.rm = TRUE),
    csw = cumsum(w),
    qp50  =  floor(csw/((N+1)/nq)) + 1
  )

#----------------------------------------------------------
#   Ratios
#----------------------------------------------------------


dgr <- dfq %>%
  select(year, p50, qp50) %>%
  group_by(year, qp50) %>%
  summarise(min  = min(p50 , na.rm = TRUE),
            mean = mean(p50, na.rm = TRUE)) %>%
  pivot_wider(names_from = qp50,
              values_from = c(min, mean),
              ) %>%
  mutate(
    gr9010 = min_9/min_1,
    gr8020 = min_8/min_2,
    gr7525 = mean_7/mean_2,
  ) %>%
  select(year, starts_with("gr")) %>%
  pivot_longer(cols = starts_with("gr")  ,
               names_to = "gr")

#----------------------------------------------------------
#   Charts
#----------------------------------------------------------

ggplot(data  = dgr,
       aes(
         x = year,
         y = value,
         color = gr
       )) +
  geom_line() +
  geom_point() +
  theme_classic()


