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

# keeping a minimum of years
overall <- dta %>%
  group_by(countrycode) %>%
  mutate( Iny =  ifelse(abs(Year - 2000) == min(abs(Year - 2000)),Year,0),
          Fny =  ifelse(abs(Year - 2015) == min(abs(Year - 2015)),Year,0),
          Iny = max(Iny),
          Fny = max(Fny)
          ) %>%
  filter( Iny <= Year & Fny >= Year) %>%
  mutate(Period  = Fny - Iny) %>%
  filter(Period > 5,
         Year == Iny | Year == Fny) %>%
  arrange(countrycode, Year) %>%
  mutate(
    Growth   = Value/lag(Value) -1,                  # abs growth
    Value0   = if_else(Year == Iny, Value,0),        # Value in first year
    Value0   = max(Value0),                          # max value in first year
    Term     = paste(Iny, Fny, sep = "-"),           # info
    GAGR     = ((Value/Value0)^(1/Period) -1),       # Annualized growth
    rem_time = if_else(15-Period < 0, 0, 15-Period), # Remaining time to 15-year period
    project  = Value*(1+GAGR)^rem_time,              # Projection
    Growthp  = ((project - Value0)/Value0)           # growth change using proection
  ) %>%
  drop_na()

clist2 <- length(unique(projecty$countrycode))



# ==== print (important stuff) ====
cat(paste(
  "Code ran:\n",
  paste0( (clist2/clist)*100," % of original countries kept under rule.\n"),
  paste("Full countries:", clist),"\n",
  paste("Used countries:", clist2))
)

