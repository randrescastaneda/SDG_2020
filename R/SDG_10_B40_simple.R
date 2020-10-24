# ==================================================
# project:       Simple B40 Story
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-10-21
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             data
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("here")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------
WDI <-
  read_csv(here("data", "WDI2020.csv"))

WDI <- WDI %>%
  rename(
    grB40       = bottom_growth,
    grmean      = total_growth,
    grmedian    = median_growth,
    countrycode = code
  ) %>%
  mutate(
    region = if_else(grepl("High Income", region),
                     "OHI",
                     as.character(region)
                     ),
    grpremium = (grB40 - grmean),
    diffabs = abs(grpremium)
  )

df <- WDI %>%
  select(region,
         country,
         countrycode,
         period,
         type,
         starts_with("gr")
  ) %>%
  pivot_longer(cols         = starts_with("gr"),
               names_to     = "growth",
               values_to    = "value",
               names_prefix = "gr") %>%
  mutate(value = value/100,
         cty   = paste0(country, " (", period, ")"))

b40_df <- df %>%
  filter(growth == "B40") %>%
  select(countrycode,
         B40 = value)

premium_df <- df %>%
  filter(growth == "premium") %>%
  select(countrycode,
         premium = value)


df <- df %>%
  left_join(b40_df, by     = "countrycode") %>%
  left_join(premium_df, by = "countrycode") %>%
  filter(growth != "median")

#----------------------------------------------------------
#  Chart
#----------------------------------------------------------

#--------- Sorted by Bottom 40 ---------

ggplot(filter(df, growth != "premium")) +
  geom_point(aes(x     = value,
                 y     = reorder(cty, B40),
                 color = growth)
             ) +
  geom_vline(xintercept = 0,
             color      = "#00B4F0") +
  theme_minimal() +
  theme(
    axis.text.y     = element_text(size = 5),
    axis.title.y    = element_blank(),
    legend.title    = element_blank(),
    legend.position = c(.8,.2)
  ) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title    = "Growth of Bottom40 and overall mean (Sorted by B40)",
    subtitle = "circa 2012-2017",
    x        = "Annualized growth"
  )




ggplot(filter(df, growth != "mean")) +
  geom_point(aes(x     = value,
                 y     = reorder(cty, premium),
                 color = growth)
             ) +
  geom_vline(xintercept = 0,
             color      = "#00B4F0") +
  theme_minimal() +
  theme(
    axis.text.y     = element_text(size = 5),
    axis.title.y    = element_blank(),
    legend.title    = element_blank(),
    legend.position = c(.8,.2)
  ) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title    = "Growth of Bottom40 and Premium (Sorted by premium)",
    subtitle = "circa 2012-2017",
    x        = "Annualized growth"
  )



