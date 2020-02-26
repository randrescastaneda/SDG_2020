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
source("R/_aux_data.R")



#----------------------------------------------------------
#   filtered dfc data
#----------------------------------------------------------

lyear <- 2015


dfc_1 <- dfc %>%
  filter(status == "OK") %>%
  pivot_wider(
    values_from = threshold,
    names_from = goal,
    names_prefix = "p",
    id_cols = c(countrycode, year)
  ) %>%
  filter(year == lyear) %>%
  # merge regions and country names
  left_join(cnames) %>%  left_join(rc) %>%
  arrange(p50)

#----------------------------------------------------------
#   Gini data
#----------------------------------------------------------

df_g <- povcalnet(fill_gaps = TRUE) %>%   # Load povcalnet data
  filter(year == lyear) %>%

  # make we only have one observation per country
  group_by(countrycode) %>%
  mutate(n = sequence(n())) %>%
  filter((n == 1) |
         (n == 3 & (coveragetype  %in% c("N", "A"))) |
         (n == 2 & datatype == "consumptioin")) %>%

  # Keep important variables
  select(countrycode,
         year,
         gini,
         mean,
         population) %>%
  arrange(countrycode)


dfc_1g <- dfc_1 %>%
  left_join(df_g) %>%
  mutate(countrycode = factor(countrycode, countrycode))


#----------------------------------------------------------
#   charts
#----------------------------------------------------------

#--------- without Gini
p_p10p90 <- ggplot(data = dfc_1g,
                   aes(x = countrycode)) +
  geom_errorbar(aes(ymin = p10,
                    ymax = p90,
                    color = region),
                width = .5) +
  geom_point(aes(y = p50)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5)
  )
p_p10p90

ggplotly(p_p10p90)


# ggplotly(p_p10p90, tooltip = "text")

#--------- with Gini

adj_scale <- 150
ggplot(data = dfc_1g) +
  geom_point(aes(x = countrycode,
                y = p50)) +
  geom_point(aes(x = countrycode,
                 y = gini*adj_scale,
                 color = region)) +
  scale_y_continuous(sec.axis = sec_axis(~./adj_scale, name = "Gini Index")) +
  geom_errorbar(aes(x = countrycode,
                    ymin = p10,
                    ymax = p90,
                    color = region),
                width = 0.5) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5)
  )

