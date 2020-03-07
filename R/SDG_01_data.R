# ==================================================
# project:       SDG 1 fixing data
# Author:        R.Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-11
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             dataframes
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("povcalnetR")
library("haven")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#--------- define steps for breaking the axis of the chart

f_steps <- function(k, zero = TRUE) {
  step <- k
  if (zero == TRUE) {
    f <- 0
  } else {
    f <- floor(min(y))
  }

  function(y) {
    seq(f, ceiling(max(y)), by = step)
  }

}

#----------------------------------------------------------
# Prepare actual data
#----------------------------------------------------------

# countries and regions
regs <- c("EAP", "ECA", "LAC", "MNA", "SAS", "SSA", "OHI")
reg <-  map(regs, get_countries)

cr <-  as_tibble(countrycode = NULL,
                 region = NULL) # country and regions


for (r in seq_along(regs)) {

  a <- tibble(countrycode = reg[[r]],
              region =  regs[r])

  cr <- bind_rows(cr, a)
}

st_year <- 1990

# Global poverty
wld <- povcalnet_wb() %>%
  filter(year > st_year, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  )

# Data at country level
cty <- povcalnet(fill_gaps = TRUE) %>%
  filter(year > st_year) %>%
  group_by(countrycode, year) %>%
  mutate(n  = n()) %>%
  filter((n == 1) |
           (n == 3 & (coveragetype  %in% c("N", "A"))) |
           (n == 2 & datatype == "consumption")) %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  ) %>%
  inner_join(cr) %>%
  mutate(text = paste0("Country: ", countryname, "\n",
                       "Region: ", region, "\n",
                       "Headcount: ", round(headcount*100, digits = 1), "%\n",
                       "Million of poor: ", poor_pop, "\n",
                       "Year: ", year, "\n"),
         regionf = as.factor(region)) %>%
  select(countrycode,
         countryname,
         year,
         mean,
         headcount,
         population,
         poor_pop,
         region, regionf,
         text)

#----------------------------------------------------------
#   Prepare forcasted data
#----------------------------------------------------------


#--------- Data at the country level

# pr_cty <- read_dta("data/projections.dta") # load data provided by Daniel from Twinning
load(file = "data/projections.Rdata")


cutyr <- 2018
pr_cty <- pr_cty %>%
  filter(growth  %in% c("2018-2023", ""),    # Filter projection growth
         gic  %in% c("l", "")) %>%           # type of GIC
  select(-matches("^FGT[12]|^.*_3|^.*_55"))

#--------- Global data

# Collapse date by alpha, extragrowth and year
pr_wld <- pr_cty %>%
  group_by(alpha, extragrowth, year) %>%
  summarise(
    # weigthed mean by pop and divide by 100
    headcount = weighted.mean(x = FGT0_19, w = pop, na.rm = TRUE)/100
  ) %>%
  ungroup() %>%
  arrange(year)

# get value of poverty for cutting year, (2018)
pr_temp <- pr_wld %>%
  filter(year == cutyr) %>%
  select(headcount) %>%
  pull()

# get combinations of alpha and extra growth from year 2019
pr_25 <- pr_wld %>%
  filter(year == cutyr + 1) %>%
  select(-headcount, -year) %>%
  mutate(
    headcount = pr_temp,   # add poverty year from 2018
    year = cutyr           # add year variable for cutyear == 2018
  )

#--------- joind actual data and projected data

pr_wld_act <- pr_wld %>%       # global projected
  filter(year > 2015) %>%      # stay with years after overlapping year (2015)
  bind_rows(pr_25) %>%         # append fake 2018 series
  bind_rows(wld) %>%           # append real data
  # Convert to factor and remove NA
  mutate(
    alpha = as.factor(ifelse(is.na(alpha), 0, alpha)),
    extragrowth = as.factor(ifelse(is.na(extragrowth), 0, extragrowth)),
    scenario = paste(alpha, extragrowth)
  )  %>%
  #filter(alpha == 0) %>%   # projection filter... This has to change for the app
  arrange(year)

rm(pr_temp, pr_25)    # remove unnecessary data


#----------------------------------------------------------
#   Data for Elbert
#----------------------------------------------------------

#--------- Country data


cty_df <- cty %>%
  mutate(
    poor_pop = round(headcount * population, 2)
  ) %>%
  select(-c(regionf, text))

write.csv(cty_df,
         file="data/SDG01_Country_data.csv",
         row.names = FALSE,
         col.names = TRUE,
         na="")

#--------- World data


wld_df <- pr_wld %>%
  mutate(case =
           case_when(
             (alpha == -2 & extragrowth == 2)    ~ "Best",
             (alpha == 2 & extragrowth == -2)    ~ "Worst",
             (alpha == 0 & extragrowth == 0)     ~ "Most likely",
             (is.na(alpha) & is.na(extragrowth)) ~ "Actual",
             TRUE ~ ""
           )) %>%
  filter(case != "") %>%
  select(-c(alpha, extragrowth)) %>%
  left_join(select(wld, poor_pop, year))

write.csv(wld_df,
          file="data/SDG01_global_data.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")



# Global poverty trend and goal
yv <- tibble( year = c(2016:2022))

wld2 <- wld %>%
  select(year, headcount) %>%
  arrange(year) %>%
  bind_rows(yv)

# liner model
lm_wld <-  lm(headcount ~ year,
             data = wld2)

lm_wld <- data.frame(wld2,
                     hc_proj = predict(lm_fit, wld2))


write.csv(lm_wld,
          file="data/lm_projection_global.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")

