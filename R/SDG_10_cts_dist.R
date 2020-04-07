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
library("Hmisc")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

eq_quantiles <- function(x, y, w, nq = 10, name = "q") {
  tryCatch(
    expr = {
      y <- enquo(y)
      w <- enquo(w)

      df <- x %>%
        arrange(!! y) %>%
        mutate(
          N  = sum(!!w),
          cw = cumsum(!!w),
          !! name :=  floor(cw/((N+1)/nq)) + 1
        ) %>%
        select(-c(N,cw))

    }, # end of expr section

    error = function(e) {
      df <- tibble(
        message = e$message
      )
    }

  ) # End of trycatch

  return(df)
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
        ) %>%
        select(povertyline, headcount, popshr, weight, welfare)

    }, # end of expr section

    error = function(e) {
      print(e$message)
    }

  ) # End of trycatch

  return(df)
}

ovv_info <- function(df, one_val_var){
  tryCatch(
    expr = {
      df %>%
        slice(1) %>%
        select(one_val_var)
    }, # end of expr section

    error = function(e) {
      tibble(
        message = e$message
      )
    } # end of finally section
  ) # End of trycatch
}



#----------------------------------------------------------
#   prepare data
#----------------------------------------------------------

# dfr <- rcv_dist(country = "COL", year = 2015,  step = 0.5, pl = 0.5)
lc <- read_rds("data/recovered_dist.rds")
lc <- Filter(function(x) !(is.null(dim(x))), lc)  # remove null element of list

# unique-value variables
one_val_var <- lc[[1]] %>%
  map(n_distinct) %>%
  imap_dfr(~tibble(value = .x,
                   var   = .y)) %>%
  filter(value == 1) %>%
  select(var) %>%
  pull()

lc_info <- lc %>%
  map(ovv_info, one_val_var)

# create variables
lc2 <- lc %>%
  map(create_welf) %>%
  map(eq_quantiles, welfare, weight, 20)

lc2 <- Filter(function(x) !(is.null(dim(x))), lc2)  # remove null element of list
write_rds(lc2, "data/cts_dist.rds")



#----------------------------------------------------------
# jumps in Gap
#----------------------------------------------------------

y <- lc$DZA1993national %>%
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
    # welfare   =  if_else (row_number() > 1,
    #                       welfare - lag(welfare),
    #                       welfare),
    #
    # # per capita
    # welfare    = welfare/weight
  ) %>%
  select(povertyline, headcount, povertygap, population, popshr, weight, welfare)

View(y)

ggplot(y, aes(x = povertyline, y = povertygap)) +
  geom_point() +
  geom_line()

ggplot(y, aes(x = povertyline, y = headcount)) +
  geom_point() +
  geom_line()





# N <-  df$population[1]
# G <-  df$povertygap[20]
# m <- df$mean[1]
# Y <- N*m
# h <- df$headcount[20]
# z <- df$povertyline[20]
# n <- N*h
# (z*(n-G*N))/n
#
#
#
#
# aa <- povcalnet_iterate(country = "COL",
#                   year = 2015,
#                   goal = .999,
#                   tolerance = 4)
#
# dta <- df %>%
#   select(welfare, weight, gini)
#
#
# haven::write_dta(dta, paste0(tdirp, "/dftest.dta"))
#
#
#
# walk(lc, ~ if (length(unique(.x$regioncode)) > 1) {
#       print(paste(unique(.x$countrycode), unique(.x$year)))
#    }
# )
#
#
#
#
#
#
# y <- deparse(substitute(y))
# y    <- x[[y]]   # welfare
#
# w <- deparse(substitute(w))
# if (w == "NULL") {
#   w = rep(1, times = length(y))
# } else {
#   w    <- x[[w]]    # weigth
# }
#
# ordy <- order(y)     # order of y
#
# w    <- w[ordy]      #order weight
# y    <- y[ordy]      # order welfare
#
# N    <- sum(w)       # population size
# cw   <- cumsum(w)    # Cumulative weights
#
#
#
#
#
#
# a <- lc %>%
#   map(~{.x[.x$q < 20,]})
#
# a <- lc %>%
#   map(~print(.x[["q"]]))
#
# a <- lc %>%
#   map(filter, q < 20)
#
# a <- lc %>%
#   map(~filter(.x$q < 20))
#
#
# a <- lc %>%
#   map(ff)
#
#
# filter(df, q < 20)
#
# ovv_info(df, one_val_var)
#
# gini(df, welfare, weight)
#
# a <- eq_quantiles(df, welfare, weight)
#
#
# lc2 %>%
#   map(eq_quantiles, welfare, weight, 20)
#
#
#
# map(lc, ~ if (is.null(dim(.x))) {
#   print(paste(unique(.x$countrycode), unique(.x$year)))
# }
# )
#
#
#
