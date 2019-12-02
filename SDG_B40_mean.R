# ==================================================
# project:       B40 and mean
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-02
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

library("tidyverse")
library("plotly")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
# Get data from Povcalnet
#----------------------------------------------------------

mdf <- povcalnet(fill_gaps = TRUE) %>%   # Load povcalnet data
  # Keep important variables
  select(contains("country"),
         contains("year"),
         starts_with("decil"),
         ppp,
         mean,
         population) %>%
  # reshape long
  pivot_longer(cols = contains("decile"),
               names_to = "decile",
               values_to = "share") %>%

  # change formating of deciles
  mutate(
    decile = as.integer(str_replace(decile, "decile([1-9]+)", "\\1")),
  ) %>%  #mean of deciles
  filter(decile <= 4) %>%
  group_by(countrycode,  year, datayear, countryname) %>%
  summarise(
    mean = mean(mean, na.rm = TRUE),
    share = sum(share, na.rm = TRUE),
    population = mean(population, na.rm = TRUE)
  )  %>%
  mutate(
    b40mean = (mean*share*40)
  ) %>%
  select(countrycode,
         countryname,
         year,
         b40mean,
         mean) %>%
  arrange(countrycode, year)



#----------------------------------------------------------
#   Parameters
#----------------------------------------------------------
