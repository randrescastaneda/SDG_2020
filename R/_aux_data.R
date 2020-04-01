# auxiliary data. This script is to re executed in other scripts

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

cnames <- povcalnet(fill_gaps = TRUE, year = 2015) %>%
  distinct(countrycode, countryname)

cr <- povcalnet() %>%
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

write_rds(cr, "data/cty_regs_names.rds")
