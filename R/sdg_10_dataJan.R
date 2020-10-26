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
library("pins")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")
board_register("rsconnect", server = "http://w0lxopshyprd1b.worldbank.org:3939/")

#----------------------------------------------------------
#   hundred people
#----------------------------------------------------------
source("R/SDG_10_hundred_people.R")

cf <- cf[, .(welfare, population = weight, countrycode)]


write_csv(cf,
          file = "data/SDG10_daily_income.csv",
          col_names = TRUE,
          na = "")


pe <- pe[, .(
  percentile = hcf,
  headcount = CSy,
  welfare,
  cumm_income = qc,
  share_income = Sy
)]

write_csv(pe,
          file = "data/SDG10_perfect_equality.csv",
          col_names = TRUE,
          na = "")


df <- pin_get("acastanedaa/percentiles_country_povcalnet", board = "rsconnect")

df <- df[, .(
  countrycode,
  year,
  percentile = quantile,
  headcount,
  share_income = Sy,
  welfare,
  cumm_income = qc
)]


write_csv(df,
          file = "data/SDG10_share_income.csv",
          col_names = TRUE,
          na = "")



df2 <- df[countrycode  %chin% c("BRA", "FIN")
          & year        %in%   c(1993, 2017)]


write_csv(df2,
          file = "data/SDG10_share_income_BRA_FIN.csv",
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
          file = "data/SDG10_change_gini.csv",
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
          file = "data/SDG10_p10p90p50_two_countries.csv",
          col_names = TRUE,
          na = "")


dfc_1 <- dfc_1 %>%
  rename(ratio_90_10 = r9010) %>%
  select(-c(lending, fcountrycode))

write_csv(dfc_1,
          file = "data/SDG10_p10p90p50_ALL_countries.csv",
          col_names = TRUE,
          na = "")



#----------------------------------------------------------
# Inequality between countries
#----------------------------------------------------------

source("R/SDG_10_global_ineq_between_countries.R")

p50d_15 <- p50d_15[, .(countrycode, meadian = p50, decile = qp50)]

write_csv(p50d_15,
          file = "data/SDG10_medians_2015.csv",
          col_names = TRUE,
          na = "")

pr      <- pr[, .(year, palma)]
write_csv(pr,
          file = "data/SDG10_palma_ratio.csv",
          col_names = TRUE,
          na = "")


dfq     <-  dfq %>%
  filter(!is.na(p50)) %>%
  select(countrycode, year, median = p50)

write_csv(dfq,
          file = "data/SDG10_medians_overTime.csv",
          col_names = TRUE,
          na = "")


#----------------------------------------------------------
#   All percentiles
#----------------------------------------------------------

dfc   <- read_rds("data/dfc.rds")
names <- read_rds("data/cty_regs_names.rds")
setDT(names)

qtile <- function(x) {
  nq  <- 10
  N   <-  length(x)
  csw <-  1:N
  qp  <-   floor(csw / ((N + 1) / nq)) + 1
  return(qp)
}

# set data.table

DT <- as.data.table(dfc)

oldn <- c("threshold", "goal")
newn <- c("percentile_value", "percentile")

setnames(DT, oldn, newn, skip_absent = TRUE)

setorder(DT, year, percentile, percentile_value)

# Sort

DT <- DT[# remove old years
  year >= 1990
  ][# filter negative values (which we should not have)
    percentile_value > 0 & !is.na(percentile_value)

  ][, # multiply by 100
    percentile := 100 * percentile

  ][, # Create deciles in each percentile
    decile_within_percentile := qtile(percentile_value),
    by = .(year, percentile)

  ][,
    headcount := NULL
    ]

# Add countryname and region
DT[names,
   on = .(countrycode),
   `:=`(
        countryname = i.countryname,
        region      = i.region
     )
   ]


write_csv(DT,
          file = "data/SDG10_percentiles_overTime.csv",
          col_names = TRUE,
          na = "")

#----------------------------------------------------------
# Non-monetary measures
#----------------------------------------------------------

source("R/SDG_10_non_monetary_Growth.R")

# Social protection coverage
coverd <- coverd %>%
  select(Year,
         countrycode,
         countryname,
         region,
         incomegroup,
         quintile,
         value,
         growth,
         val) %>%
  rename(mean2007 = val) #This mean is used for sorting in the graph

write_csv(coverd,
          file = "data/SDG10_social_protection_cover.csv",
          col_names = TRUE,
          na = "")

# Social Protection: CPI social protection rating
CPIss <- CPIss %>%
  select(
    Year,
    countrycode,
    countryname,
    region,
    incomegroup,
    value,
    v2007,
    growth,
    win,
    loss,
    initial
  ) %>%
  rename(value2007 = v2007)  #for sorting

write_csv(CPIss,
          file = "data/SDG10_social_protection_rating.csv",
          col_names = TRUE,
          na = "")

# Remittances
remit_from <- remit_from %>%
  select(year,
         countrycode,
         countryname,
         region,
         incomegroup,
         value,
         growth,
         span) %>%
  rename(time_span = span) # For graphs of growth I keep those w/ an span greater than 5 years

write_csv(remit_from,
          file = "data/SDG10_remittances_origin.csv",
          col_names = TRUE,
          na = "")

remit_to <- remit_to %>%
  select(year,
         countrycode,
         countryname,
         region,
         incomegroup,
         value,
         growth,
         span) %>%
  rename(time_span = span) # For graphs of growth I keep those w/ an span greater than 5 years

write_csv(remit_to,
          file = "data/SDG10_remittances_desstination.csv",
          col_names = TRUE,
          na = "")

# policies for social inclusion/equity
CPIinc <- CPIinc %>%
  select(
    Year,
    countrycode,
    countryname,
    region,
    incomegroup,
    value,
    v2007,
    growth,
    win,
    loss,
    initial
  ) %>%
  rename(value2007 = v2007)  #for sorting


write_csv(CPIinc,
          file = "data/SDG10_social_inclusion_rating.csv",
          col_names = TRUE,
          na = "")

# AID
AID <- AID %>%
  filter(incomegroup != "High income") %>%
  group_by(countrycode) %>%
  filter(row_number() == n()) %>%
  select(countrycode,
         countryname,
         region,
         incomegroup,
         v2007,
         v2017,
         change,
         growth) %>%
  rename(value2007 = v2007,
         value2017 = v2017)

write_csv(AID,
          file = "data/SDG10_aid.csv",
          col_names = TRUE,
          na = "")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Bottom 40   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


source("R/SDG_10_B40_simple.R")


write_csv(df,
          file = "data/SDG10_B40_simple.csv",
          col_names = TRUE,
          na = "")

cb40 <- read_csv("data/PEB_b40_profiling.csv") %>%
  janitor::clean_names() %>%
  select(region,
         countrycode = code,
         countryname,
         characteristic = precase,
         incomegroup,
         year,
         share_b40 = b40,
         share_t60 = t60
         )

write_csv(cb40,
          file = "data/SDG10_B40_characteristics.csv",
          col_names = TRUE,
          na = "")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Share below 50% of median   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here("R", "SDG_10_50percent_median.R"))


write_csv(d45,
          file = "data/SDG10_50percent_median_2000-2017.csv",
          col_names = TRUE,
          na = "")
