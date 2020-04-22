# auxiliary data. This script is to re executed in other scripts
library("tidyverse")
library("povcalnetR")
#--------- Load data

load("data/dfc.RData")
load("data/dfr.RData")

#--------- median

dfc50 <- dfc %>%
  filter(goal == 50) %>%
  select(countrycode, year,
         median = threshold)

#--------- names of regions

regions  <- c("ECA", "MNA", "SSA", "LAC", "OHI", "SAS", "EAP")

region_country <- function(x) {
  cnames <- get_countries(x)
  df <- tibble(
    countrycode = cnames,
    region = x
  )
  return(df)
}


rc <- map_dfr(regions, region_country)

#--------- countrynames

if (interactive()) {

  cnames <- povcalnet(fill_gaps = TRUE, year = 2015) %>%
    distinct(countrycode, countryname)

  cr <- povcalnetR::povcalnet() %>%
    distinct(countrycode, countryname, regioncode) %>%
    rename(region = regioncode) %>%
    mutate(
      regionname = case_when(
        region == "EAP" ~ "East Asia and Pacific",
        region == "ECA" ~ "Europe and Central Asia",
        region == "LAC" ~ "Latin America and the Caribbean",
        region == "MNA" ~ "Middle East and North Africa",
        region == "OHI" ~ "Other High Income countries",
        region == "SAS" ~ "South Asia",
        region == "SSA" ~ "Sub-saharan Africa",
        TRUE ~ ""
      ),
      countryname = gsub("(.*)(,.*)", "\\1", countryname)
    )

  inc_gr <- WDI::WDI(indicator = c("SP.POP.TOTL"),
                     start = 2018,
                     end = 2018,
                     extra = TRUE) %>%
    mutate(
      iso3c  = if_else(iso2c == "MK", "MKD", as.character(iso3c)),
      income = if_else(iso2c == "MK", "Upper middle income", as.character(income)),
    ) %>%
    distinct(countrycode = iso3c,
           incomegroup = income,
           lending)

  cr <- cr %>%
    left_join(inc_gr, by = "countrycode")

  write_rds(cr, "data/cty_regs_names.rds")
}
