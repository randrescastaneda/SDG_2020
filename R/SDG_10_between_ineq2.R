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

# set data.table
yrs <- c(1993, 2002, 2010, 2015, 2017)

DT <- as.data.table(dfc)
cr <- as.data.table(cr)
setnames(DT, "threshold", "pv")

# Sort
setorder(DT, year, goal, pv)

DT <- DT[
  # remove old years
  year >= 1990
  ][
    # filter negative values (which we should not have)
    pv > 0 & !is.na(pv)
  ][,
    # multiply by 100
    goal := 100*goal

  ][
    ,# Create deciles in each percentile
    qp := qtile(pv),
    by = .(year, goal)

  ][
    ,
    headcount := NULL
  ][
    # Merge country names and regions
    cr,
    on = .(countrycode)
    ][
      year  %in% yrs
    ]


#----------------------------------------------------------
#   relative ratio to percentile
#----------------------------------------------------------

qps <- 1
pc  <- 50

DTF <- DT[
  qp == qps
  & goal == pc
  ][
    ,
    .SD[which.max(pv)],
    by = .(year)
  ]

setorder(DTF, year, pv)


DT <-
  DT[
  DTF,
  on = .(year),
  pvd := i.pv
  ][
    ,
    ratio := pv/pvd
  ][
    , # Create Text variable
    text := paste0("Country: ", countryname, "\n",
                   "Year: ", year, "\n",
                   "Ratio:", ratio, "\n")
  ][,
    N := .N,
    by = .(year, goal)
  ]

setorder(DT, year, goal, pv)

DT[
  ,
  i := rowid(year, goal)
  ][
    ,
    rpos := i/N
  ]


pq <- ggplot(DT[goal == pc],
             aes(
               x = rpos,
               y = ratio,
               color = factor(year)
             )
) +
  geom_line() +
  geom_point(
    aes(text = text)
  ) +
  theme_minimal() +
  labs(
    title = "Evolution of inequality across countries ",
    x = "Relative position of country (percentile)",
    y = "Ratio percentile/selected decile"
  )


plotly::ggplotly(pq, tooltip = "text")



pt <- ggplot(DT[goal == pc & pv < 75],
       aes(
         x = pv,
         y = ratio,
         color = factor(year),
         text = text
       )
       ) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(trans = 'log2')


plotly::ggplotly(pt, tooltip = "text")



ggplot(DT[goal == pc & year == 2017],
       aes(
         x = pv,
         y = ratio
       )
       ) +
  geom_point() +
  geom_smooth() +
  theme_minimal()





#----------------------------------------------------------
#   Typical country in each decile
#----------------------------------------------------------

DA <- DT[

  # select b10 and t10
  qp  %in% c(1, 10)
][
  , # Get the median by groups
  med := median(pv, na.rm = TRUE),
  by = .(year, goal, qp)

][
  , # min abs diff between median and pv by groupw
  dfmed := min(abs(pv - med)),
  by = .(year, goal, qp)

][
  # Filter those of min diff
  dfmed == abs(pv - med)

][
  , # select the min pv in case of tie in medians (when even No. of obs.)
  .SD[which.min(pv)],
  by = .(year, goal, qp)
]

DA <- dcast.data.table(DA,
                       year + goal  ~ qp,
                       value.var = c("countrycode", "pv"))

DA[
  ,
  ratio := pv_10/pv_1
]


ggplot(filter(DA, goal == 50),
       aes(
         x = year,
         y = ratio
       )) +
  geom_line() +
  geom_point()










#
# mx <- DT[goal == pc & (year == min(year) | year == max(year))
#          ][
#            pv == max(pv)
#            ][
#              ,
#              pv
#            ]
# mn <- DT[
#   goal == pc & (year == min(year) | year == max(year))
#   ][
#     pv == min(pv)
#   ][
#     ,
#     pv
#   ]
#
#
# DD <- DT[goal == pc & (year == min(year) | year == max(year))
#          ][
#            ,
#            `:=`(
#              maxr = max(ratio)
#            ),
#            by = .(year, qp)
#          ][
#            maxr == ratio
#          ][
#            ,
#            yr := ifelse(year == min(year), "y1", "y2")
#          ]
# DD <- dcast.data.table(DD,
#                       qp ~ yr,
#                        value.var = c("ratio"))
#
#
# DD[
#   ,
#   pv := seq(..mn,..mx, length.out = 10)
# ]
#
#
#
# ggplot(DT[goal == pc]
#        ) +
#   geom_line(aes(
#     x = pv,
#     y = ratio,
#     color = factor(year)
#   )) +
#   geom_point(aes(
#     x = pv,
#     y = ratio,
#     color = factor(year)
#   )) +
#   geom_ribbon(data = DD,
#               aes(
#                 ymin = y2,
#                 ymax = y1,
#                 x = pv
#               )
#               ) +
#   theme_minimal()
#





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







#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

# set data.table
yrs <- c(1993, 2002, 2010, 2015, 2017)

DT <- as.data.table(dfc)
cr <- as.data.table(cr)
setnames(DT, "threshold", "pv")

# Sort
setorder(DT, year, goal, pv)

DT <- DT[
  # remove old years
  year >= 1990
][
  # filter negative values (which we should not have)
  pv > 0 & !is.na(pv)
][,
  # multiply by 100
  goal := 100*goal

][
  ,# Create deciles in each percentile
  qp := qtile(pv),
  by = .(year, goal)

][
  ,
  headcount := NULL
][
  # Merge country names and regions
  cr,
  on = .(countrycode)
][
  year  %in% yrs
]



#--------- parameters ---------

# calculation
ratio_calc <- function(DT,
                       nm,
                       dn,
                       pc   = 50,
                       ms   = mean,
                       yrs  = c(1993, 2017)
) {

  calc <- paste0(".(", ms, "  = ", ms, "(pv, na.rm = TRUE))")
  calc <- parse(text = calc)


  DW <- DT[
    # Filter selected percentile
    goal == pc
    & year  %in% yrs

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
  ][
    ,
    label := paste0(paste(nm, collapse = "-"), "/", paste(dn, collapse = "-"))
  ]

  # Reshape to wide
  DW <- dcast.data.table(DW,
                         year + label ~ gr,
                         value.var = ms)

  # Ratio
  DW[
    ,
    ratio := round(nm/dn, digits = 2)
  ]

  return(DW)

}

#

pc <- 50       # percentile
ms <- "mean"   # measure
nm <- c(9, 10)      # numerator
dn <- c(1, 4)       # denominator

yrs <- c(1993, 2017)


df <- ratio_calc(DT = DT,
                 nm = nm,
                 dn = dn,
                 ms = ms,
                 pc = pc,
                 yrs = yrs)

rc_args <-
  list(

    nm = list(
      c(9,9),
      c(8,8),
      c(7,7),
      c(9,10)
    ),
    dn = list(
      c(1,1),
      c(2,2),
      c(3,3),
      c(1,4)
    )
  )




lf <- pmap_df(rc_args, ratio_calc,
              pc = pc,
              ms = ms,
              yrs = yrs,
              DT = DT)


ggplot(lf,
       aes(
         x     = factor(label),
         y     = ratio,
         color = factor(year),
         group = factor(year)
       )
) +
  geom_line() +
  geom_point() +
  theme_minimal()







nm <- c(8,8)
dn <- c(1,2)

nmp <- ord_nums( unique(nm) )
dnp <- ord_nums( unique(dn) )

paste0(paste(nmp, collapse = "-"), "/", paste(dnp, collapse = "-"))




ord_nums <- function(n){

  ord <- ifelse(n %in% c(11,12,13),  "th",
                ifelse(
                  n %% 10 == 1, 'st',
                  ifelse(
                    n %% 10 == 2, 'nd',
                    ifelse(
                      n %% 10 == 3 , 'rd',  "th"
                    )
                  )
                )
  )

  return(paste0(n, ord))
}

ord_nums(nm)
