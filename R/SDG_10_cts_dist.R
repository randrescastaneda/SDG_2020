# ==================================================
# project:       Find where the action is happening in the distribution
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-03
# Modification Date:
# Script version:    01
# References:
#
#
# Output:           charts
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


gini <- function(x, y, w) {
  y <- deparse(substitute(y))
  w <- deparse(substitute(w))

  if (length(w) == 0) {
    w = rep(1, times = length(y))
  } else {
    w    <- x[[w]]    # weigth
  }

  y    <- x[[y]]   # welfare
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



create_welf <- function(x) {
  tryCatch(
    expr = {
      df <- x %>%
        filter(regioncode == "XX") %>%
        arrange(povertyline) %>%
        mutate(
          population = population*1e6,
          popshr     = if_else (row_number() > 1,         # population share
                                headcount - lag(headcount),
                                headcount),
          weight     = population*popshr, # people below threshold

          # cumm welfare
          welfare   = povertyline*(population*headcount - population*povertygap),

          # cnvert to bin from cumm
          welfare   =  if_else (row_number() > 1,
                                welfare - lag(welfare),
                                welfare),

          # per capita
          welfare    = welfare/weight
        )
    }, # end of expr section

    error = function(e) {
      df <- tibble(
        message = e$message
      )
    }

  ) # End of trycatch

  return(df)
}



#----------------------------------------------------------
#   prepare data
#----------------------------------------------------------

# dfr <- rcv_dist(country = "COL", year = 2015,  step = 0.5, pl = 0.5)
lc <- read_rds("data/recovered_dist.rds")
lc <- lc %>%
  map(create_welf)

# df <- lc$COL2015

gt <- lc %>%   # Gini total
  map_dbl(gini,welfare, weight) %>%
  tibble(gini = .,
        id = attr(., "names")
        )








#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------
N <-  df$population[1]
G <-  df$povertygap[20]
m <- df$mean[1]
Y <- N*m
h <- df$headcount[20]
z <- df$povertyline[20]
n <- N*h
(z*(n-G*N))/n




aa <- povcalnet_iterate(country = "COL",
                  year = 2015,
                  goal = .999,
                  tolerance = 4)

dta <- df %>%
  select(welfare, weight, gini)


haven::write_dta(dta, paste0(tdirp, "/dftest.dta"))



walk(lc, ~ if (length(unique(.x$regioncode)) > 1) {
      print(paste(unique(.x$countrycode), unique(.x$year)))
   }
)


