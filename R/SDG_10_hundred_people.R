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

c1 <- read_rds("data/recovered_dist.rds")$CRI2018national
c1$countrycode <- "CRI"

c2 <- read_rds("data/recovered_dist.rds")$FIN2018national
c2$countrycode <- "FIN"

c3 <- read_rds("data/recovered_dist.rds")$COL2018national
c3$countrycode <- "COL"

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
#   Perfect equalit
#----------------------------------------------------------

setDT(c1)
wm <- c1[, weighted.mean(welfare, weight)]

# perfect equality df
pe <- data.table(
  weight = c1$weight,
  welfare = wm
)


#----------------------------------------------------------
# Share of income
#----------------------------------------------------------

nq <- 100
tl <- 2.5  # Tolerance
cf[
  ,
  n := as.double(.N/(nq)),
  by = .(countrycode)
  ][
    ,
    nq := if_else(n > (tl), (nq),
                  ceiling( ((nq)*n) / (tl))
    )
    ]

df <- cf[
  order(countrycode, welfare)
  ][,
    pc := cut(x = headcount,
              breaks = seq(0, 1, by = 1/min(nq)),
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

# Add variable for cummulative income
df[ ,
    qc := factor(
      if_else(between(CSy, 0, .2), 1,
              if_else(between(CSy, .2, .4, incbounds = FALSE), 2,
                      if_else(between(CSy, .4, .6), 3,
                              if_else(between(CSy, .6, .8, incbounds = FALSE), 4,5)
                      )
              )
      ),
      labels = c("< 20%", "< 40%", "< 60%", "< 80%", "< 100%")
    )
    ]

#--------- Make sure Pens parade holds
# given that the distribution was recoevered, it could be the case that some
# particular rates are just simply not avaialble in the distribution and thus
# it is impossible to know some values

#--------- Share of welfare

# lagged and lead obs
df[order(countrycode, pc),
  `:=`(lgSy = shift(Sy,
                  n = 1,
                  fill = 0,
                  type = "lag"),
       ldSy = shift(Sy,
                     n = 1,
                     fill = 0,
                     type = "lead"),
       ld2Sy = shift(Sy,
                    n = 2,
                    fill = 0,
                    type = "lead"),
       i     = .I
       ),
   by = .(countrycode)
   ]

# Get differences
df[
  ,
  c("dlgSy", "dldSy", "dld2Sy") := {
    dlgSy  <- Sy   - lgSy
    dldSy  <- Sy   - ldSy
    dld2Sy <- ldSy - ld2Sy
    list(dlgSy, dldSy, dld2Sy)
  }
]

# Fix problematic cases

df[
  ,
  Sy := if_else(dldSy > 0 & ldSy != 0, (ldSy + lgSy)/2,
                 if_else(dld2Sy > 0 & dld2Sy <= abs(dldSy), Sy + abs(dld2Sy)*.99,
                         Sy)
                 ),
  by = .(countrycode)
  ]

#--------- welfare

# lagged and lead obs
df[order(countrycode, pc),
  `:=`(lgwelfare = shift(welfare,
                  n = 1,
                  fill = 0,
                  type = "lag"),
       ldwelfare = shift(welfare,
                     n = 1,
                     fill = 0,
                     type = "lead"),
       ld2welfare = shift(welfare,
                    n = 2,
                    fill = 0,
                    type = "lead"),
       i     = .I
       ),
   by = .(countrycode)
   ]

# Get differences
df[
  ,
  c("dlgwelfare", "dldwelfare", "dld2welfare") := {
    dlgwelfare  <- welfare   - lgwelfare
    dldwelfare  <- welfare   - ldwelfare
    dld2welfare <- ldwelfare - ld2welfare
    list(dlgwelfare, dldwelfare, dld2welfare)
  }
]

# Fix problematic cases

df[
  ,
  welfare := if_else(dldwelfare > 0 & ldwelfare != 0, (ldwelfare + lgwelfare)/2,
                 if_else(dld2welfare > 0 & dld2welfare <= abs(dldwelfare), welfare + abs(dld2welfare)*.99,
                         welfare)
                 ),
  by = .(countrycode)
  ]

df <- df[, .(countrycode, pc, headcount, Sy, welfare, qc)]


# gtest <- ggplot(
#   data = df[countrycode == "FIN"],
#   aes(
#     x = as.factor(headcount),
#     y = Sy,
#     fill = qc
#   )
# ) +
#   geom_bar(stat  = "identity")+
#   theme_classic() +
#   theme(
#     legend.title =  element_blank(),
#     axis.text.x = element_blank()
#   ) +
#   labs(
#     y = "Share of total income",
#     x = "Percentile"
#   ) +
#   scale_y_continuous(labels = scales::percent)
#
# plotly::ggplotly(gtest)
#



# [order(countrycode, headcount)
#        ][,
#          hc := factor(headcount, headcount)
#        ]


# ggplot(
#   data = df[countrycode == "COL"],
#   aes(
#     x = as.factor(headcount),
#     y = Sy,
#     fill = qc
#   )
# ) +
#   geom_bar(stat = "identity")+
#   theme_classic() +
#   theme(
#     legend.title =  element_blank(),
#     axis.text.x = element_blank()
#   ) +
#   labs(
#     y = "Share of total income",
#     x = "Percentile"
#   ) +
#   scale_y_continuous(labels = scales::percent)
#
# ggplot(
#   data = df[countrycode == "FIN"],
#   aes(
#     x = as.factor(headcount),
#     y = welfare,
#     fill = qc
#   )
# ) +
#   geom_bar(stat = "identity")+
#   theme_classic() +
#   theme(
#     legend.title =  element_blank(),
#     axis.text.x = element_blank()
#   ) +
#   labs(
#     y = "Daily income",
#     x = "Percentile"
#   ) +
#   scale_y_continuous(labels = scales::dollar)
#
