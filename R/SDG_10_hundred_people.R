# ==================================================
# project:       Sotry of hundred people
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-05-19
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts and data
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

# labels for charts
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        'too big!')
                                 )
                          )
                   )
  return(labels)
}

# Weighted median
weighted.median <- function(x, w) {
  w <- w[order(x)]
  x <- x[order(x)]

  prob <- cumsum(w)/sum(w)
  ps   <- which(abs(prob - .5) == min(abs(prob - .5)))
  return(x[ps])
}


#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")
dfr <- read_rds("data/dfr.rds")
# lc <- read_rds("data/cts_dist.rds")

#----------------------------------------------------------
#   Set up
#----------------------------------------------------------

c1 <- read_rds("data/cts_dist.rds")$CRI2015national
c1$countrycode <- "CRI"

c2 <- read_rds("data/cts_dist.rds")$FIN2015national
c2$countrycode <- "FIN"

c3 <- read_rds("data/cts_dist.rds")$COL2015national
c3$countrycode <- "COL"


nq   <- 100 # No. of quantiles
cf <- bind_rows(c1,c2, c3) %>%
  arrange(countrycode, welfare) %>%
  group_by(countrycode) %>%
  mutate(
    N  = sum(weight, na.rm = TRUE),
    cw = cumsum(weight),
    q  =  floor(cw/((N+1)/nq)) + 1,
  ) %>%
  ungroup() %>%
  mutate(
    cty = fct_reorder(countrycode, welfare)
  )


#----------------------------------------------------------
# Hundred people
#----------------------------------------------------------

# medians
md <- cf %>%
  group_by(cty) %>%
  summarise(med = weighted.median(welfare, weight)) %>%
  arrange(med)

cts <- md %>%
  pull(cty)

#
ggplot(data = filter(cf, welfare < 100),
       aes(x = welfare,
           weight = weight,
           fill  = cty)) +
  geom_histogram(bins = 100,
                 position="identity",
                 alpha = .5) +
  # geom_density(alpha=0.6)    +
  scale_y_continuous(labels = addUnits) +
  scale_x_continuous(labels = scales::dollar) +
  scale_fill_manual(values = palette,
                     breaks = cts) +
  geom_vline(data = md,
             aes(xintercept = med,
                 color      = cty),
             linetype = "dashed") +
  scale_color_manual(values = palette[1:3],
                     breaks = cts) +
  theme_classic() +
  theme(
    legend.title = element_blank()
  ) +
  labs(y = "Population",
       x = "Daily income")


ggplot(data = filter(cf, welfare < 100,
                     countrycode != "COL"),
       aes(x = welfare,
           weight = weight,
           fill  = cty)) +
  geom_histogram(bins = 100,
                 position="identity",
                 alpha = .5) +
  # geom_density(alpha=0.6)    +
  scale_y_continuous(labels = addUnits) +
  scale_x_continuous(labels = scales::dollar) +
  scale_fill_manual(values = palette[1:3],
                     breaks = cts) +
  geom_vline(data = md,
             aes(xintercept = med,
                 color      = cty),
             linetype = "dashed") +
  scale_color_manual(values = palette,
                     breaks = cts) +
  theme_classic() +
  theme(
    legend.title = element_blank()
  ) +
  labs(y = "Population",
       x = "Daily income")







#----------------------------------------------------------
# Organize in a different way
#----------------------------------------------------------




#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------



