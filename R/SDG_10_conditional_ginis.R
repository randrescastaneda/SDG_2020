# ==================================================
# project:       Analysis of Gini
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-07
# Modification Date:
# Script version:    01
# References:
#
#
# Output:
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("CGPfunctions")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

gini <- function(x, y, w = NULL) {

  y <- deparse(substitute(y))
  w <- deparse(substitute(w))

  y    <- x[[y]]   # welfare

  if (w == "NULL") {
    w = rep(1, times = length(y))
  } else {
    w    <- x[[w]]    # weigth
  }

  ordy <- order(y)     # order of y

  w    <- w[ordy]      #order weight
  y    <- y[ordy]      # order welfare

  N    <- sum(w)       # population size
  Y    <- sum(y*w)     # total welfare

  cw   <- cumsum(w)    # Cumulative weights
  cy   <- cumsum(y*w)  # Cumulative welfare

  sn   <-  w/N         # share of population
  my   <- weighted.mean(y, w, na.rm = TRUE)

  i    <- (2*cw - w + 1)/2
  t2   <- y*(N - i + 1)
  gini <- 1+(1/N) - (2/(my*N^2))*sum(t2*w)
  return(gini)
}


lc2 <- read_rds("data/cts_dist.rds")
cr  <- read_rds("data/cty_regs_names.rds")
#----------------------------------------------------------
#   calculations
#----------------------------------------------------------


g0100 <- lc2 %>%   # Gini total
  map_dbl(gini,welfare, weight) %>%
  tibble(g0100 = .,
         id = attr(., "names")
  )

g0095 <- lc2 %>%
  map(~.x[.x[["q"]] < 20,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0095 = .,
         id = attr(., "names")
  )

g0595 <- lc2 %>%
  map(~.x[.x[["q"]] > 1 & .x[["q"]] < 20,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0595 = .,
         id = attr(., "names")
  )


g0090 <- lc2 %>%
  map(~.x[.x[["q"]] < 19,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0090 = .,
         id = attr(., "names")
  )


g1090 <- lc2 %>%
  map(~.x[.x[["q"]] > 2 & .x[["q"]] < 19,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g1090 = .,
         id = attr(., "names")
  )

gt_w <- g0100 %>%
  inner_join(g0095)   %>%
  inner_join(g0595) %>%
  inner_join(g0090)   %>%
  inner_join(g1090) %>%
  select(id, everything()) %>%
  filter_if(is.numeric, all_vars(.>0 & . < 1)) %>%
  mutate(
    countrycode = gsub("([A-Z]+)(.*)", "\\1", id),
    year        = gsub("(\\D+)([0-9]+)(\\D+)", "\\2", id),
    coverage    = gsub("([A-Z]+)([0-9]+)([[:alpha:]])", "\\3", id)
  ) %>%
  left_join(cr, by = "countrycode")

gt_l <- gt_w %>%
  pivot_longer(cols           = starts_with("g"),
               names_to       = "gini_type",
               names_prefix   = "g",
               values_to      = "value",
               values_drop_na = TRUE) %>%
  mutate(gini_type     = ordered(gini_type, levels = c("0100", "0095", "0595", "0090", "1090")),
         countrycode   = as.factor(countrycode),
         value         = round(value, digits = 3)) %>%
  group_by(countrycode, year, coverage) %>%
  mutate(
    rank = order(value)
  ) %>%
  ungroup()



gt_l <- gt_w %>%
  group_by(countrycode, year, coverage) %>%
  mutate_at(
  # across(starts_with("g"), ~ order(.x), .names = "r{col}")  for dplyr 1.0.0
  # create ranking
    vars(starts_with("g")), list(r = ~order(.))
  ) %>%
  rename_at(
    vars(ends_with("_r")),
    list(~ paste("r", gsub("_r", "", .), sep = "_"))
    ) %>%
  pivot_longer(cols           = starts_with("g"),
               names_to       = "gini_type",
               names_prefix   = "g",
               values_to      = "value",
               values_drop_na = TRUE) %>%
  mutate(gini_type     = ordered(gini_type, levels = c("0100", "0095", "0595", "0090", "1090")),
         countrycode   = as.factor(countrycode),
         value         = round(value, digits = 3)) %>%
  group_by(countrycode, year, coverage) %>%
  mutate(
    rank = order(value)
  ) %>%
  ungroup() %>%
  pivot



#----------------------------------------------------------
# Charts
#----------------------------------------------------------



gdf_p <- gt_l %>%
  filter(year == 2015, region  %in% c("SSA"))

newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = value,
                Grouping    = countrycode)







xvar <- "g1090"
ggplot(data = gt_w,
       aes(x = get(xvar),
           y = gt,
           color = region)
       ) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(.1, .7) + ylim(.1, .7)




#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

