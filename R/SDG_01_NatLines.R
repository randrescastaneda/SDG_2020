#=======================================#
#       National lines growth           #
#=======================================#

# Libraries

library("tidyverse")
library("data.table")

#===== Get data an minor clean up =====

data <- readr::read_csv("data/dataNatPovCountryLevel.csv")

# I'll Stick to comparable data
# clean year
dta <- data %>%
  filter(Series == "SI.POV.NAHC") %>%
  rename(Value = Data,
         countrycode = Country) %>%
  mutate(
    Year = str_extract(Time,"([0-9]+)")
  ) %>%
  mutate_at( c("Year", "Value"), as.numeric) %>%
  select(-Time) %>%
  setDT()

# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()

cols  <- c("regioncode", "countryname")
icols <- paste0("i.", c("region", "countryname"))

dta <- dta[cr,
          on = c("countrycode"),
          (cols)  := mget(icols)
          ]

dta <- dta[complete.cases(dta),]

# drop countries with a sinlge data point
dta <- dta %>%
  group_by(countrycode) %>%
  filter( n() > 1) %>%
  ungroup()

clist <- length(unique(dta$countrycode))

#====== Proyection ======

#------  Using yearly growth

# Parameters
year1   <- 2000
year2   <- 2015
yspam   <- year2 - year1
min_per <- 5


# keeping a minimum of years

overall <- dta %>%
  group_by(countrycode) %>%
  mutate( Iny =  if_else(abs(Year - year1) == min(abs(Year - year1)),Year,0),
          Fny =  if_else(abs(Year - year2) == min(abs(Year - year2)),Year,0),
          Iny = max(Iny),
          Fny = max(Fny)
          ) %>%
  filter( Iny <= Year & Fny >= Year) %>%
  mutate(Period  = Fny - Iny) %>%
  filter(Period > min_per,
         Year == Iny | Year == Fny) %>%
  arrange(countrycode, Year) %>%
  mutate(
    Growth      = Value/lag(Value) -1,                  # abs growth
    Value0      = if_else(Year == Iny, Value,0),        # Value in first year
    Value0      = max(Value0),                          # max value in first year
    Term        = paste(Iny, Fny, sep = "-"),           # info
    GAGR        = ((Value/Value0)^(1/Period) -1),       # Annualized growth
    rem_time    = if_else(yspam-Period < 0, 0, yspam-Period), # Remaining time to 15-year period
    project     = Value*(1+GAGR)^rem_time,              # Projection
    Growthp     = ((project - Value0)/Value0),          # growth change using proection
    dec         = if_else(Growthp < 0, 1, 0),                 # Dummy for decrease or increase pov
    countryname = gsub("(.*)(,.*)", "\\1", countryname) # remove part of the name after comma
  ) %>%
  ungroup() %>%
  drop_na()


# ==== print (important stuff) ====
# cat(paste(
#   "Code ran:\n",
#   paste0( (clist2/clist)*100," % of original countries kept under rule.\n"),
#   paste("Full countries:", clist),"\n",
#   paste("Used countries:", clist2))
# )

