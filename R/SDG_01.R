# ==================================================
# project:       SDG 1 viz
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-11
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             Viz
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("zoo")
library("plotly")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#
#----------------------------------------------------------

# countries and regions
regs <- c("EAP", "ECA", "LAC", "MNA", "SAS", "SSA")
reg <-  map(regs, get_countries)

cr <-  as_tibble(countrycode = NULL,
                 region = NULL) # country and regions


for (r in seq_along(regs)) {

  a <- tibble(countrycode = reg[[r]],
              region =  regs[r])

  cr <- bind_rows(cr, a)
}


# Global poverty
wld <- povcalnet_wb() %>%
  filter(year > 1989, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  )

# Data at country level
cty <- povcalnet(fill_gaps = TRUE) %>%
  filter(year > 1989) %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  ) %>%
  inner_join(cr)





headcount_col <- "#E69F00"

ggplot() +
  geom_line(data = wld,
            aes(x = year,  y = headcount),
            color = headcount_col) +
  geom_point(data = cty,
             aes(x = year, y = headcount, size = poor_pop, color = region)) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  ) +
  labs(y = "Poverty rate (%)",
       x = "")



