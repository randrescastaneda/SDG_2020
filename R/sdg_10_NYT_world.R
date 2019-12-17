# ==================================================
# project:       replicate NYT article for the whole world
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-05
# Modification Date:
# Script version:    01
# References:
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
source("R/_aux_data.R")
load("../dwd/data/dfc.RData")
load("../dwd/data/dfr.RData")


#----------------------------------------------------------
#
#----------------------------------------------------------


dft <- dfc %>% filter(status == "OK") %>%
  # merge regions and country names
  left_join(cnames) %>%  left_join(rc) %>%
  bind_rows(dfr) %>% # append regional and global data
  group_by(countrycode) %>%
  filter((year == min(year)) |
           (year == max(year))) %>%
  ungroup() %>%
  filter(goal != 99)

dft_f <- dft %>%
  filter(is.na(region))

v_breaks <- seq(floor(min(dft$threshold)),
    ceiling(max(dft$threshold)),
    by = 10)



ggplot() +
  # draw the original data series with grey
  geom_line(data = dft,
            aes(year, threshold, group = countrycode),
            colour = alpha("grey", 0.7)) +
  # color only the filtered data (regions and world)
  geom_line(data = dft_f,
            aes(year, threshold, color = countrycode)) +
  facet_wrap(~goal,
             nrow = 1,
             strip.position = "bottom") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 5,
                               angle = 90),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) +
  scale_y_continuous(name = "2011 PPP USD a day",
                     #breaks = v_breaks,
                     # breaks = f_steps(10, zero = FALSE),
                     breaks = function(y) seq(floor(min(y)), ceiling(max(y)), by = 10),
                     labels = scales::dollar)
