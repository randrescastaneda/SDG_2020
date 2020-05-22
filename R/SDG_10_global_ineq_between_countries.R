# ==================================================
# project:       evoluation of inequality between countries
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-05-05
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

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")
dfr <- read_rds("data/dfr.rds")


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

nq <- 10
dfq <- dfc %>%
  filter(status == "OK") %>%
  pivot_wider(
    values_from = threshold,
    names_from = goal,
    names_prefix = "p",
    id_cols = c(countrycode, year)
  ) %>%
  group_by(year) %>%
  arrange(year, p50) %>%
  mutate(
    w  = 1,
    N  = sum(w, na.rm = TRUE),
    csw = cumsum(w),
    qp50  =  floor(csw/((N+1)/nq)) + 1
  ) %>%
  ungroup()

#----------------------------------------------------------
#   Ratios
#----------------------------------------------------------


dgr <- dfq %>%
  select(year, p50, qp50) %>%
  group_by(year, qp50) %>%
  summarise(min  = min(p50 , na.rm = TRUE),
            mean = mean(p50, na.rm = TRUE)) %>%
  pivot_wider(names_from  = qp50,
              values_from = c(min, mean),
              ) %>%
  mutate(
    gr9010 = min_9/min_1,
    gr8020 = min_8/min_2,
    gr7525 = mean_7/mean_2,
  ) %>%
  select(year, starts_with("gr")) %>%
  pivot_longer(cols     = starts_with("gr")  ,
               names_to = "gr")

#----------------------------------------------------------
#   distribution of medians
#----------------------------------------------------------
setDT(dfq)
p50d_15 <- dfq[year == 2015,
            .(countrycode, p50, qp50)]

maxq <- p50d_15[,
             .(maxq = max(p50),
               minq = min(p50)),
             by = .(qp50)
             ]


# distribution of median in 2015 with vertical
# lines in 20th and 80th percentile

pp50d_15 <- ggplot(p50d_15,
       aes(x = p50)) +
  geom_histogram(aes(y    = ..density..),
                 binwidth = .5,
                 colour   = "black",
                 fill     = "#F6CF71",
                 alpha    = .3) +
  geom_density(alpha = .2,
               fill  = "#1D6996") +
  labs(x = "Medians of the world") +
  theme_minimal()





# pp50d_15+
#   annotate("rect", xmin = 0,
#            xmax = maxq[qp50 == 2, maxq],
#            ymin = 0,
#            ymax = Inf,
#            alpha = .3,
#            fill      = "#CC503E")

# pp50d_15

# distribution of medians over time
# pp50d <- ggplot(filter(dfq, !is.na(p50)),
#                 aes(x = p50,
#                     fill  = factor(year))) +
#   geom_density(alpha = .2)+
#   theme_minimal() +
#   labs(x = "Distribution of medians over time") +
#   theme(
#     legend.title = element_blank(),
#     legend.position = c(.8, .5),
#     legend.direction = "horizontal"
#   )



#----------------------------------------------------------
#   Palma Ratio
#----------------------------------------------------------
p50d <- dfq[,
            .(year, p50, qp50)]


ty <- p50d[,
           .(ty = sum(p50, na.rm = TRUE)),
           by = .(year)]



s40 <- p50d[ty, on = "year", ty := i.ty
            ][
              qp50 <= 4,
              .(s40 = sum(p50, na.rm = TRUE)/mean(ty, na.rm = TRUE)),
              by = .(year)
            ]

s90 <- p50d[ty, on = "year", ty := i.ty
            ][
              qp50 == 10,
              .(s90 = sum(p50, na.rm = TRUE)/mean(ty, na.rm = TRUE)),
              by = .(year)
              ]
# Palma ratio
pr <- s90[s40, on = "year", s40 := i.s40
             ][,
               palma := s90/s40]

#----------------------------------------------------------
#   Charts
#----------------------------------------------------------

# ggplot(data  = filter(dgr, gr == "gr9010"),
#        aes(
#          x = year,
#          y = value,
#          color = gr
#        )) +
#   geom_line() +
#   geom_point() +
#   theme_classic()


