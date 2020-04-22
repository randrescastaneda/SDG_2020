# ==================================================
# project:       Global gini
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-14
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
cr  <- read_rds("data/cty_regs_names.rds")
lc  <- read_rds("data/cts_dist.rds")

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------
df <- povcalnetR::povcalnet(fill_gaps = TRUE) %>%
  group_by(countrycode, coveragetype, datatype) %>%
  mutate(
    median = zoo::na.approx(median, year, na.rm = FALSE)
  ) %>%
  ungroup()

gmed <- df %>%
  mutate(
    w = 1
  ) %>%
  group_by(year) %>%
  summarise(gini_unweighted = gini(y = median, w = w),
            gini_weighted  = gini(y = median, w = population)) %>%
  pivot_longer(cols = starts_with("gini_"),
               names_to =  "type",
               values_to = "gini",
               names_prefix = "gini_")


#----------------------------------------------------------
#   plots
#----------------------------------------------------------
p_gini_med <- ggplot(
       data = filter(gmed, type == "unweighted"),
       aes(x = year,
           y = gini,
           color = type)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(.2,.1)
  ) +
  labs(x = "Year",
       y = "Gini coef.",
       title = "Gini coef. of medians of all countries over time",
       subtitle = "line-up years")



#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------




















# df <- lc %>%
#   map_dbl(~mean(.x[.x[["q"]]  %in%  c(10,11), "welfare"]$welfare,
#                 na.rm = TRUE)) %>%
#   tibble(welfare = .,
#          id = attr(., "names")
#   )%>%
#   mutate(
#     countrycode = gsub("([A-Z]+)(.*)", "\\1", id),
#     year        = gsub("(\\D+)([0-9]+)(\\D+)", "\\2", id),
#     coverage    = gsub("([A-Z]+)([0-9]+)([[:alpha:]])", "\\3", id)
#   ) %>%
#   left_join(cr, by = "countrycode")
#
#
# df %>%
#   group_by(year) %>%
#   summarise(gini = gini(welfare))
#
