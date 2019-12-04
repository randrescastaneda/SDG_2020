# ==================================================
# project:       P90 p10 and p50
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-04
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
#   Aux data
#----------------------------------------------------------

load("data/dfc.RData")

df_t <- dfc %>%
  filter(status == "OK") %>%
  pivot_wider(
    values_from = threshold,
    names_from = goal,
    names_prefix = "p",
    id_cols = c(countrycode, year)
  ) %>%
  filter(year == 2015) %>%
  mutate(countrycode = factor(countrycode, countrycode))


ggplot(data = df_t) +
  geom_point(aes(x = countrycode,
                y = p50)) +
  geom_errorbar(aes(x = countrycode,
                    ymin = p10,
                    ymax = p90),
                width = 0.5) +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5)
  )
