# ==================================================
# project:       growth of b40 in two periods
# Author:        David Vargas (modified by Andres Castaneda)
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             data for charts
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library(wbstats)
library(tidyverse)
library(data.table)
library(scales)
library(hrbrthemes)
library(here)

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


# functions

source(here("R", "panel_WDI.R")) # Wrapper to wb function from wbstat

#----------------------------------------------------------
# parameter
#----------------------------------------------------------


# parameters
ordervariable <-
  "incomegroup" # how to group countries, works with region, incomegroup or both c("region","incomegroup")
sortvariable <-
  "OLDGrowth" # How to sort values within groups Growth40, Growth, diff, diffabs?
rankvariable <-
  c("diff", "OLDGrowth") # How to rank countries: Growth40, Growth, diff, diffabs?
filename <- "" # root file name


# ## Keep countries in older points
# list2017 <- c("CHN", "BEL", "ECU", "KAZ", "BTN", "KHM", "BOL", "BRA", "RUS",
#               "COL", "PER", "CHL", "URY", "MKD", "THA", "MDA", "GEO", "VNM",
#               "PAN", "UKR", "IDN", "SLV", "UGA", "TZA", "NOR", "IRN", "PAK",
#               "TGO", "ROU", "POL", "CHE", "LKA", "PHL", "FIN", "ARG", "DOM",
#               "DEU", "BEL", "KGZ", "RWA", "NLD", "USA", "ALB", "GRB", "PRT",
#               "HUN", "LVA", "ISL")


# ------------------ code ---------------------#


# Load Share Prosperity Data
WDI <-
  read.csv2(
    here("data", "WDI2020.csv"),
    sep = ",",
    dec = ".",
    stringsAsFactors = F
  )

WDI <- WDI %>%
  rename(Growth40 = bottom_growth) %>%
  rename(Growth = total_growth) %>%
  rename(GrowthMedian = median_growth) %>%
  rename(countrycode = code) %>%
  mutate(
    region = ifelse(grepl("High Income", region), "OHI", as.character(region)),
    diff = (Growth40 - Growth),
    diffabs = abs(diff)
  )

# Load the old WDI data
WDI2017 <-
  read.csv2(
    here("data", "WDI2017.csv"),
    sep = ",",
    dec = ".",
    stringsAsFactors = F
  )

WDI2017 <- WDI2017 %>%
  rename(
    countryname = Country.Name,
    countrycode = Country.Code,
    indicatorID = Indicator.Code,
    indicator = Indicator.Name,
  ) %>%
  gather("year", "value", X1960:X2016) %>%
  extract(year, into = "Year", regex = "([0-9]+)") %>%
  mutate(Year = as.numeric(as.character(Year)))

# I'll keep just those indicators I care for
WDI2017 <- WDI2017 %>%
  filter(indicatorID %in% c("SI.SPR.PC40.ZG", "SI.SPR.PCAP.ZG")) %>%
  drop_na() %>%
  select(-indicator, -countryname) %>%
  spread(indicatorID, "value") %>%
  rename(
    OLDGrowth40 = SI.SPR.PC40.ZG,
    OLDGrowth = SI.SPR.PCAP.ZG,
    OLDyear = Year
  ) %>%
  mutate(
    OLDdiff = (OLDGrowth40 - OLDGrowth),
    OLDdiffabs = abs(OLDdiff)
  )


# join data

WDI <- full_join(WDI, WDI2017, by = "countrycode") %>%
  select(-region)


# -- Add Region ID
cr <- read_rds(here("data", "cty_regs_names.rds")) %>%
  setDT() %>%
  select(-lending)

WDI <- left_join(WDI, cr, by = "countrycode") %>%
  select(-country) %>%
  rename(country = countryname) %>%
  mutate(
    country = if_else(countrycode == "KHM", "Cambodia", country),
    region = if_else(countrycode == "KHM", "EAP", region),
    incomegroup = if_else(countrycode == "KHM", "Lower middle income", incomegroup),
    regionname = if_else(countrycode == "KHM", "East Asia and Pacific", regionname)
  )

# =============#
#   plots     #
# =============#

### 2017 points ###

# data <- WDI %>%
# filter(countrycode %in% list2017)

#
# data <- WDI %>%
#   filter(OLDdiff > 0 & !is.na(diff & OLDdiff))

data <- WDI

data <-  data %>%
  mutate(
    ordervar = !! sym(ordervariable),
    ordervar = factor(x = ordervar,
                      levels = c("Low income", "Lower middle income",
                                 "Upper middle income", "High income")
                      ),
    sortvar  =  !! sym(sortvariable)
    ) %>%
  group_by(ordervar) %>%
  arrange(sortvar) %>%
  mutate(
    yid = row_number()
  ) %>%
  ungroup()


# data$id <- seq(1, nrow(data))

# empty space between regions
empty_bar        <- 1
to_add           <- data.frame(matrix(NA, empty_bar * length(unique(data$ordervar)), ncol(data)))
colnames(to_add) <- colnames(data)
to_add$ordervar  <- rep(unique(data$ordervar), each = empty_bar)
data             <- rbind(data, to_add)
data             <- arrange(data, ordervar, sortvar)
data$id          <- seq(1, nrow(data))

data <- data %>%
  mutate(
    Growth40 = as.numeric(as.character(Growth40)),
    Growth = as.numeric(as.character(Growth))
  ) %>%
  drop_na()

