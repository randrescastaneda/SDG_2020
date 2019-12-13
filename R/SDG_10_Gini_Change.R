# ==================================================
# project:       change in Gini from 2000
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-04
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
library("plotly")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------




#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
source("R/_aux_data.R")


#----------------------------------------------------------
# Gini data
#----------------------------------------------------------

minyear <- 2000

df_g <- povcalnet(fill_gaps = TRUE) %>%   # Load povcalnet data
  filter(year >= minyear) %>%
  group_by(countrycode) %>%
  filter((year == min(year)) |
           (year == max(year))) %>%
  group_by(countrycode, year) %>%
  mutate(n  = n()) %>%
  filter((n == 1) |
           (n == 3 & (coveragetype  %in% c("N", "A"))) |
           (n == 2 & datatype == "consumption")) %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(x  = sequence(n())) %>%
  pivot_wider(names_from = x,
              values_from = c(gini, year),
              id_cols = countrycode) %>%
  # merge regions and country names
  left_join(cnames) %>%  left_join(rc) %>%
  drop_na() %>%
  mutate(
    text = paste0("County: ", countryname, "\n",
                  "Range: ", year_1, "-", year_2, "\n",
                  "Region: ", region, "\n")
  ) %>%
  ungroup()

#--------- Charts


p_g <- ggplot(data = df_g,
              aes(text = text)) +
  geom_point(aes(x = gini_1,
                 y = gini_2,
                 color = region)) +
  geom_abline(intercept = 0 ,
              slope = 1,
              color = "grey50") +
  labs(x = "Gini\n(circa 2000)",
       y = "Gini\n(circa 2015)") +
  scale_x_continuous(limits = c(.25, .6)) +
  scale_y_continuous(limits = c(.25, .6)) +
  theme_classic() +
  theme(
    legend.position = "",
  )
p_g

ggplotly(p_g, tooltip = "text")
