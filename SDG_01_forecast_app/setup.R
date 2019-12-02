# ==================================================
# project:       SDG 1 viz
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-11
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             Viz
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")
library("povcalnetR")
library("haven")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
# Prepare actual data
#----------------------------------------------------------

# countries and regions
regs <- c("EAP", "ECA", "LAC", "MNA", "SAS", "SSA")
reg <-  map(regs, get_countries)

cr <-  as_tibble(countrycode = NULL,
                 region = NULL) # country and regions


for (r in seq_along(regs)) {

  a <- tibble(countrycode = reg[[r]],
              region =  regs[r])

  cr <- bind_rows(cr, a)
}


# Global poverty
wld <- povcalnet_wb() %>%
  filter(year > 1989, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  )

# Data at country level
cty <- povcalnet(fill_gaps = TRUE) %>%
  filter(year > 1989) %>%
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
         regionf = as.factor(region))


#----------------------------------------------------------
#   Prepare forcasted data
#----------------------------------------------------------


#--------- Data at the country level

pr_cty <- read_dta("../data/projections.dta") # load data provided by Daniel from Twinning

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
  ) %>%  ungroup() %>% arrange(year)

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
    extragrowth = as.factor(ifelse(is.na(extragrowth), 0, extragrowth))
  )  %>%
  arrange(year)


rm(pr_temp, pr_25)    # remove unnecessary data


#----------------------------------------------------------
#   Data for app
#----------------------------------------------------------

alpha <- unique(pr_wld_act$alpha)

extragrowth <- unique(pr_wld_act$extragrowth)
names(extragrowth) <- paste0(extragrowth, "%")


