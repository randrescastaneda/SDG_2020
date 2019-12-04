# auxiliary data. This script is to re executed in other scripts

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

