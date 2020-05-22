# ==================================================
# project:       Data for Jan
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-05-21
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             csv files
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
#   hundred people
#----------------------------------------------------------
source("R/SDG_10_hundred_people.R")
cf <- cf[, .(welfare, population = weight, countrycode)]

write_csv(cf,
          path = "data/SDG10_daily_income.csv",
          col_names = TRUE,
          na = "")

df <- df[, .(countrycode, percentile = pc, headcount,
             share_income = Sy, welfare, cumm_income = qc )]

write_csv(df,
          path = "data/SDG10_share_income.csv",
          col_names = TRUE,
          na = "")


#----------------------------------------------------------
# Change in Gini
#----------------------------------------------------------

source("R/SDG_10_Gini_Change.R")

df_g <- df_g %>%
  filter(gini_1 != gini_2) %>%
  rename(gini_2000 = gini_1, gini_2018 = gini_2)

write_csv(df_g,
          path = "data/SDG10_change_gini.csv",
          col_names = TRUE,
          na = "")

#----------------------------------------------------------
# P10 P50 and P90 charts
#----------------------------------------------------------
source("R/SDG_10_p10p90p50.R")
dfc_2c <- dfc_2c %>%
  rename(ratio_90_10 = r9010) %>%
  select(-c(lending, fcountrycode, countryx))

write_csv(dfc_2c,
          path = "data/SDG10_p10p90p50_two_countries.csv",
          col_names = TRUE,
          na = "")


dfc_1 <- dfc_1 %>%
  rename(ratio_90_10 = r9010) %>%
  select(-c(lending, fcountrycode))

write_csv(dfc_1,
          path = "data/SDG10_p10p90p50_ALL_countries.csv",
          col_names = TRUE,
          na = "")



#----------------------------------------------------------
# Inequality between countries
#----------------------------------------------------------

source("R/SDG_10_global_ineq_between_countries.R")

p50d_15 <- p50d_15[, .(countrycode, meadian = p50, decile = qp50)]

write_csv(p50d_15,
          path = "data/SDG10_medians_2015.csv",
          col_names = TRUE,
          na = "")

pr      <- pr[, .(year, palma)]
write_csv(pr,
          path = "data/SDG10_palma_ratio.csv",
          col_names = TRUE,
          na = "")


dfq     <-  dfq %>%
  filter(!is.na(p50)) %>%
  select(countrycode, year, median = p50)

write_csv(dfq,
          path = "data/SDG10_medians_overTime.csv",
          col_names = TRUE,
          na = "")

