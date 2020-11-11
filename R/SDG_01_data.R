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
library("data.table")
library("povcalnetR")
library("haven")
library("janitor")
library("broom")

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

#--------- Comparability database ---------

comparable_path <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/506801/povcalnet_comparability.csv"

comparable <- read_csv(comparable_path) %>%
  mutate(
    datatype     = if_else(datatype == 1,"consumption",  "income"),
    coveragetype = case_when(
      coveragetype == 1 ~ "U",
      coveragetype == 2 ~ "R",
      coveragetype == 3 ~ "N",
      coveragetype == 4 ~ "A",
      TRUE              ~ ""
    )
  )

server <-  NULL
#--------- countries and regions ---------

regs <- c("EAP", "ECA", "LAC", "MNA", "SAS", "SSA", "OHI")
reg <-  map(regs, get_countries)

cr <- read_rds("data/cty_regs_names.rds")

# cr <-  as_tibble(countrycode = NULL,
#                  region = NULL) # country and regions
#
#
# for (r in seq_along(regs)) {
#
#   a <- tibble(countrycode = reg[[r]],
#               region =  regs[r])
#
#   cr <- bind_rows(cr, a)
# }

st_year <- 1990

# Global poverty
wld <- povcalnet_wb(server = server) %>%
  filter(year > st_year, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 5)
  )

# Data at country level
cty <- povcalnet(server = server,
                 fill_gaps = TRUE) %>%
  left_join(comparable,
            by = c("countrycode", "year", "datatype", "coveragetype")
  ) %>%
  filter(year > st_year) %>%
  group_by(countrycode, year) %>%
  mutate(n  = n(),
         poor_pop  = round(headcount * population, 3),
         headcount = round(headcount, 5)
         ) %>%
  filter(  (n == 1)
         | (n == 3 & (coveragetype  %in% c("N", "A")))
         | (n == 2 & datatype == "consumption")
         ) %>%
  left_join(cr) %>%
  mutate(text = paste0("Country: ", countryname, "\n",
                       "Region: ", region, "\n",
                       "Headcount: ", round(headcount*100, digits = 1), "%\n",
                       "Million of poor: ", round(poor_pop, 2), "\n",
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
         text,
         comparability) %>%
  arrange(countrycode, year) %>%
  ungroup()

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
  filter(year > 2017) %>%      # stay with years after overlapping year (2015)
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Get comparable series   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cty_comp <- cty %>%
  mutate(
    comp_orig = comparability
  ) %>%
  group_by(countrycode) %>%
  add_count(comp_orig) %>%
  fill(comparability) %>%
  filter(comparability == max(comparability,
                              na.rm = TRUE)
         ) %>%
  mutate(n2 = n(),
         n  = if_else(is.na(comp_orig), NA_integer_, n)) %>%
  filter(n2 != 1,
         n  != 1) %>%
  select(-c(n, n2))



#----------------------------------------------------------
#   Countries trends typology
#----------------------------------------------------------

y_pred <- tibble(year = c(1990:2250))

cty_pred <- cty_comp %>%
  nest(data = -countrycode) %>%                        # Split in several dataframes
  mutate(fit       = map(data, ~lm(headcount ~year, data = .)),  # regression
         headcount = map(fit, predict, newdata = y_pred),        # predict in new data
         beta      = map(fit, ~tidy(.)[["estimate"]][2]),        # extract beta
         year      = map(y_pred, cbind),                         # add years
         beta      = map(rep(beta, length(y_pred)), cbind)       # add beta
  ) %>%
  unnest(c(headcount, year, beta)) %>%
  select(countrycode, year, headcount, beta) %>%
  as.data.table()

zero_pov <- 1/1e4

# clean data

cty_p <- cty_pred[beta < -1e-3
                  & headcount  %inrange% c(zero_pov:100)
                  ][,                    # only those that elimiate pov.
                    .SD[which.max(year)],
                    by = countrycode
                    ][,
                      countrycode := as.character(countrycode)  # for plotting, not factor
                      ][
                        year > 2018                         # for plotting
                        ]


# join regions names

setDT(cr)
cols  <- c("region", "countryname", "regionname")
icols <- paste0("i.", c("region", "countryname", "regionname"))

cty_p <- cty_p[cr,
           on = c("countrycode"),
           (cols)  := mget(icols)]


# create subsample for repel labels
set.seed(10101)
cty_pred2 <- cty_p[sample(nrow(cty_p), 20),] %>%
  mutate(
    text = paste0(countrycode, " (", year, ")")
  )


povch <- cty_comp %>%
  group_by(countrycode) %>%
  mutate(
    maxy = max(year),
    miny = min(year)
  ) %>%
  filter(  year == maxy
         | year == miny ) %>%
  mutate(
    povchange = headcount - lag(headcount,
                                order_by = year),
    povchange = if_else(is.na(povchange),
                        lead(povchange,
                             order_by = year),
                        povchange),
    povtrend = case_when(
      povchange    > 0   ~ "Bad",
      povchange    < 0   ~ "Good",
      povchange    == 0  ~ "Same",
      TRUE ~ ""
    )
  ) %>%
  arrange(countrycode, year) %>%
  ungroup()


cty_bad <- povch %>%
  filter(
    povtrend == "Bad"
    & region != "OHI"
    & headcount > 0.05
    ) %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(
    no_poor = headcount*population*1e6,
    gr_pp = (no_poor/lag(no_poor))^(1/(year-lag(year)))-1
  )

# bad_ctrs <- cty_bad %>%
#   filter(!(is.na(gr_pp))) %>%
#   select(countryname, countrycode, gr_pp, no_poor, headcount, region) %>%
#   mutate(
#     countryname = gsub("(.*)(,.*)", "\\1", countryname) # remove part of the name after comma
#   )

setDT(cty_comp)
bad_ctrs <- cty_pred[
  # get country codes of those for which povert increased in last
  # comparable spell
  beta > 0,
  .(countrycode = unique(countrycode))
  ][ # get data with poverty changes for only those countries in which
    # poverty increased.
    cty_comp,
    on = "countrycode",
    nomatch = 0
    ][,
      ymm := if_else(year == min(year) | year == max(year), 1, 0),
      by = .(countrycode)
      ][
        ymm == 1
        ][,
          no_poor := headcount*population*1e6,
          ][
            order(countrycode, year),
            gr_pp := (no_poor/shift(no_poor, 1, NA, "lag"))^
              (1/(year-shift(year, 1, NA, "lag")))-1,
            by = .(countrycode)
            ][
              region != "OHI" & headcount > 0.05 & !is.na(gr_pp)
              ][
                cr,
                on = c("countrycode"),
                (cols)  := mget(icols)
                ]



