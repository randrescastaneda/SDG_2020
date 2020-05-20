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

c1 <- read_rds("data/recovered_dist.rds")$CRI2015national
c1$countrycode <- "CRI"

c2 <- read_rds("data/recovered_dist.rds")$FIN2015national
c2$countrycode <- "FIN"

c3 <- read_rds("data/recovered_dist.rds")$COL2015national
c3$countrycode <- "COL"

nq   <- 100 # No. of quantiles
cf <- as.data.table(bind_rows(c1,c2, c3))

cf <- cf[order(countrycode, welfare)
         ][,
             cty := fct_reorder(countrycode, welfare)
             ]

#----------------------------------------------------------
# Stacking people
#----------------------------------------------------------

# medians

md <- cf[
  ,
  .(med = weighted.median(welfare, weight)),
  by = .(cty)
  ][
    order(med)
    ]

cts <- md[, cty]

# ggplot(data = filter(cf, welfare < 100),
#        aes(x = welfare,
#            weight = weight,
#            fill  = cty)) +
#   geom_histogram(bins = 100,
#                  position="identity",
#                  alpha = .5) +
#   # geom_density(alpha=0.6)    +
#   scale_y_continuous(labels = addUnits) +
#   scale_x_continuous(labels = scales::dollar) +
#   scale_fill_manual(values = palette[1:3],
#                      breaks = cts) +
#   geom_vline(data = md,
#              aes(xintercept = med,
#                  color      = cty),
#              linetype = "dashed") +
#   scale_color_manual(values = palette[1:3],
#                      breaks = cts) +
#   theme_classic() +
#   theme(
#     legend.title = element_blank()
#   ) +
#   labs(y = "Population",
#        x = "Daily income")

#----------------------------------------------------------
# Share of income
#----------------------------------------------------------

df <- cf[
  order(countrycode, welfare)
  ][,
    pc := cut(x = headcount,
              breaks = seq(0, 1, by = 1/nq),
              labels = FALSE),
    by = .(countrycode)
    ][ ,
       .SD[which.min(headcount)],
       by = .(countrycode, pc)
       ][ ,
          c("yl", "CSy") :=  {

            # Parameters
            Y    <-  population*mean          # total welfare
            Nl   <-  population*(pc/nq)       # population in the percentile
            zl   <-  Nl*povertyline           # line times the population
            AGl  <-  povertygap*population*povertyline  # Total gap in each line

            # total income per percentile
            yl   <-  zl - AGl
            yl   <-  yl*(Y/max(yl))           # normalize income to max

            # Cummulative share of income of q in total welfare
            CSy  <-   yl/Y

            #return
            list(yl, CSy)
          },
          by = .(countrycode)
          ][
            order(countrycode, pc),
            Sy := CSy - shift(CSy,  # share of income per quantile in total income
                              n = 1,
                              fill = 0,
                              type = "lag"),
            by = .(countrycode)
            ][
              order(countrycode, pc, Sy)
              ]

#--------- Make sure Pens parade holds
# given that the distribution was recoevered, it could be the case that some
# particular rates are just simply not avaialble in the distribution and thus
# it is impossible to know some values

df <- df[order(countrycode, pc),
   lagSy := Sy - shift(Sy,
                       n = 1,
                       fill = 0,
                       type = "lag"),
   by = .(countrycode)
   ][,
     Sy := if_else(lagSy < 0, shift(Sy, n = 1, fill = 0, type = "lag")  #previous observation
                   + 0.005*Sy,                                 # plus some difference
                   Sy),
     by = .(countrycode)
     ]


pens <- ggplot(
  data = df[countrycode == "COL"],
  aes(
    x = as.factor(headcount),
    y = Sy
  )
) +
  geom_bar(stat = "identity")

plotly::ggplotly(pens)

pens <- ggplot(
  data = cf,
  aes(
    x = as.factor(headcount),
    y = welfare
  )
) +
  geom_bar(stat = "identity")

plotly::ggplotly(pens)
