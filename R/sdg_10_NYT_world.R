# ==================================================
# project:       replicate NYT article for the whole world
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-05
# Modification Date:
# Script version:    01
# References:     https://www.nytimes.com/2014/04/23/upshot/the-american-middle-class-is-no-longer-the-worlds-richest.html?rref=upshot
#
#
# Output:             Chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")
library("povcalnetR")
library("gghighlight")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

#--------- define steps for breaking the axis of the chart

f_steps <- function(k, zero = TRUE) {
  step <- k
  if (zero == TRUE) {
    f <- 0
  } else {
    f <- floor(min(y))
  }

  function(y) {
    seq(f, ceiling(max(y)), by = step)
  }

}

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------

source("R/utils.R")
load("../dwd/data/dfc.RData")
load("../dwd/data/dfr.RData")
cr  <- read_rds("data/cty_regs_names.rds")
# dfc <- read_rds("data/dfc.rds")


#----------------------------------------------------------
# prep data
#----------------------------------------------------------


dft <- dfc %>%
  # merge regions and country names
  left_join(cr, by = "countrycode") %>%
  bind_rows(dfr) %>% # append regional and global data
  mutate(
    year = if_else(year == 1990, 1993, year)
  ) %>%
  filter((year == 1993) | (year == 2015),
         status  %in%   c("OK", NA),
         goal != 99)

dft_f <- dft %>%
  filter(is.na(region))

v_breaks <- seq(floor(min(dft$threshold)),
                ceiling(max(dft$threshold)),
                by = 10)

#----------------------------------------------------------
#   all countries and regions
#----------------------------------------------------------


p_nyt <-  ggplot() +
  # draw the original data series with grey
  geom_line(data = dft,
            aes(x = year,
                y = threshold,
                group = countrycode),
            colour = alpha("grey", 0.7)) +
  # color only the filtered data (regions and world)
  geom_line(data = dft_f,
            aes(x = year,
                y = threshold,
                color = countrycode)) +
  facet_wrap(~goal,
             nrow = 1,
             strip.position = "bottom") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 5,
                               angle = 90),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = c(.1,.8),
    legend.title = element_blank()
  ) +
  scale_y_continuous(name = "2011 PPP USD a day",
                     #breaks = v_breaks,
                     # breaks = f_steps(10, zero = FALSE),
                     breaks = function(y) seq(floor(min(y)), ceiling(max(y)), by = 10),
                     labels = scales::dollar)

ggplotly(p_nyt)

# ggplotly(p_nyt, tooltip = "text")



#----------------------------------------------------------
#   filtered charts
#----------------------------------------------------------

dftf <- dft %>%
  arrange(countrycode, goal, year) %>%
  group_by(countrycode, goal) %>%
  mutate(
    perform = if_else(
      threshold - lag(threshold) < 0,
      "Bad",
      "Good"
    ),
    perform = if_else(is.na(perform), lead(perform), perform)
  ) %>%
  group_by(countrycode) %>%
  add_count(perform, name = "np") %>%
  add_count(name = "nc") %>%
  mutate(
    shr_perform = np/nc,
    bad_perf    = if_else(perform == "Bad" & shr_perform >= .5, 1, 0),
    bad_perf    = max(bad_perf)
  ) %>%
  filter(!is.na(bad_perf),
         incomegroup == "Low income") %>%
  ungroup()


p_nyt_f <-  ggplot(data = filter(dftf, bad_perf == 1),
                   aes(x = year,
                       y = threshold,
                       group = countrycode,
                       color = region)
                   ) +
  # draw the original data series with grey
  geom_line(size = 1) +
  facet_wrap(~goal,
             nrow = 1,
             strip.position = "bottom") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 5,
                               angle = 90),
    strip.background = element_blank(),
    strip.placement = "outside",
    legend.position = c(.1,.8),
    legend.title = element_blank()
  ) +
  scale_y_continuous(name = "2011 PPP USD a day",
                     #breaks = v_breaks,
                     # breaks = f_steps(10, zero = FALSE),
                     breaks = function(y) seq(floor(min(y)), ceiling(max(y)), by = 10),
                     labels = scales::dollar)

p_nyt_f

ggplotly(p_nyt_f)

