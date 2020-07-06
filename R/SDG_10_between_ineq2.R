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


qtile <- function(x) {
  nq  <- 10
  N   <-  length(x)
  csw <-  1:N
  qp  <-   floor(csw/((N+1)/nq)) + 1
  return(qp)
}


#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

# set data.tabl
DT <- dfc %>%
  rename(pv = threshold) %>%
  as.data.table()

# Sort
setorder(DT, year, goal, pv)

# convert 99.99 to 100 for simplicity
DT[
  goal == 99.99,
  goal := 100
  ]

# Create deciles in each percentile
DT[
  ,
  qp := qtile(pv),
  by = .(year, goal)
  ]


DR <-
  DT[
  ,
  .(
    min  = min(pv, na.rm = TRUE),
    mean = mean(pv, na.rm = TRUE),
    max  = max(pv, na.rm = TRUE)
  ),
  by = .(year, goal, qp)
  ]



# convert to wide
DW <- dcast(DR,
            year + goal ~ qp,
            value.var = c("mean", "min", "max"))


#--------- parameters ---------

pc <- 10       # percentile
ms <- "mean"   # measure
nm <- c(9, 10)      # numerator
dn <- c(1, 4)       # denominator


# calculation

calc <- paste0(".(", ms, "  = ", ms, "(pv, na.rm = TRUE))")
calc <- parse(text = calc)


DW <- DT[
  # Filter selected percentile
  goal == pc

  ][
  # classify numerator and denominator
  ,
  gr := ifelse(qp %between% nm, "nm",
               ifelse(qp %between% dn, "dn", NA_character_ )
               )

  ][
  # remove not necessary data
  !is.na(gr)
  ][
    , # Make requested calculation
    eval(calc),
    by = .(year, goal, gr)
  ]

# Reshape to wide
DW <- dcast(DF,
            year ~ gr,
            value.var = ms)

# Ratio
DW[
  ,
  ratio := nm/dn
  ]

#
# var1 <- paste0(ms, "_", nm)
# var2 <- paste0(ms, "_", dn)
#
# DF <- DW[
#   goal == pc,
#   .(
#     year = year,
#     ratio = get(var1)/get(var2)
#     )
#   ]
#




ggplot(DF,
       aes(
         x = year,
         y = ratio)
       )+
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(y = "Evolution of ratio") +
  theme(
    legend.title = element_blank()
  )




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


# plots
ggplot(filter(DT, goal == 10 & year == 2015),
       aes(x = pv)
) +
  geom_histogram(aes(y    = ..density..),
                 binwidth = .5,
                 colour   = "black",
                 fill     = "#F6CF71",
                 alpha    = .3) +
  geom_density(alpha = .2,
               fill  = "#1D6996") +
  labs(x = "Medians of the world") +
  theme_minimal()



ggplot(filter(DT, goal == 90 & !is.na(pv)),
       aes(x = pv,
           fill  = factor(year)
       )
) +
  geom_density(alpha = .2)+
  theme_minimal() +
  labs(x = "Distribution of medians over time") +
  theme(
    legend.title = element_blank(),
    legend.position = c(.8, .5),
    legend.direction = "horizontal"
  )


