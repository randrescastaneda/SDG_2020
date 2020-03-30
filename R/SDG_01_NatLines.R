#=======================================#
#       National lines growth           #
#=======================================#

# Libraries
library(devtools)
library(caTools)

library(wbstats)
library(tidyverse)
library(data.table)
library(plyr)

library(plotly)
library(ggplot2)
library(ggforce)
library(hrbrthemes)
library(viridis)


#===== Get data an minor clean up =====

data <- data.table(read.csv2("dataNatPovCountryLevel.csv", sep = ","))

# I'll Stick to comparable data
dta <- data.table(filter(data, Series == "SI.POV.NAHC"))

# clean year 
dta$Year = str_extract(dta$Time,"([0-9]+)")
dta$Time <- NULL
dta <- dplyr::rename(dta, Value=Data)

dta$Year <-as.numeric(as.character(dta$Year))
dta$Value <-as.numeric(as.character(dta$Value))


# -- Add Region ID
# Download country mappings
countries <- data.table(wbcountries())
# Set keys to join the data sets
setkey(dta, Country)
setkey(countries, iso3c)
# Add regions to the data set, but remove aggregates
dta <- countries[dta][ ! region %in% "Aggregates"]

dta <- dplyr::rename(dta, countrycode=iso3c)
dta <- dplyr::rename(dta, regioncode=adminID)

dta <- dta[complete.cases(dta),]

# drop countries with a sinlge data point
dta <- dta %>% 
  group_by(countrycode) %>% 
  filter( n() > 1)

clist <- length(unique(dta$countrycode))

#====== Proyection ======

# ------ Calcule yearly growth 

yearly <- ddply(dta,"countrycode",transform,
                Growth=c(NA,exp(diff(log(Value)))-1))

#------  Using yearly growth

# keeping a minimum of years
projecty <- yearly %>%
  group_by(countrycode) %>% 
  mutate( Iny =  ifelse(abs(Year - 2000) == min(abs(Year - 2000)),Year,0) ) %>% 
  mutate( Fny =  ifelse(abs(Year - 2015) == min(abs(Year - 2015)),Year,0) ) %>% 
  mutate( Iny = max(Iny)) %>% 
  mutate( Fny = max(Fny)) %>% 
  filter( Iny <= Year & Fny >= Year) %>% 
  dplyr:: mutate(Period = abs(Year[1] - Year[dplyr::n()]) ) %>% 
  filter(Period > 5)

clist2 <- length(unique(projecty$countrycode))

# # calculate Average growth rate
# projecty <- projecty %>%
#   group_by(countrycode) %>% 
#   mutate(Grate = mean(Growth, na.rm=TRUE)) %>%
#   mutate(percount = Period) %>%
#   mutate(project = Value)

# keep meaningull vars
projecty <- projecty %>% 
  select(countrycode, regioncode, Year, Iny, Fny, Value, Period)

#=== Calculate overall change ====

overall <- projecty %>% 
  group_by(countrycode) %>% 
  arrange(Year) %>% 
  filter( Year == Iny | Year == Fny)


overall <- ddply(overall,"countrycode",transform,
                 Growth=c(NA,exp(diff(log(Value)))-1))


# original value as column and "odd" reshape
overall <- overall %>%
  group_by(countrycode) %>% 
  mutate( Value0 = ifelse(Year == Iny, Value,0) ) %>% 
  mutate( Value0 = max(Value0))

overall <- overall[complete.cases(overall),]

# save term as a string

overall <- overall %>% 
  mutate(Term = paste(Iny, Fny, sep = "-"))

#==== projection ====

# --- Calculate Compond Average Growth Rate
overall <- overall %>%
  mutate(GAGR = ((Value/Value0)^(1/Period) -1) )


# # --- Calculate Simple Average Growth Rate
# overall <- overall %>% 
#   mutate(GAGR = ( Growth/Period ))


# ---- Projection 

overall <- overall %>% mutate( percount = Period ) %>% mutate( project = Value )
 
for(i in 1:15){
  overall <- overall %>%
    group_by(countrycode) %>%
    mutate(
      project = ifelse(percount < 15, project*(1+GAGR),project)
    ) %>%
    mutate(
      percount = percount + 1
    )
}

# growth change using proection

overall <- overall %>% 
  mutate( Growthp = ((project - Value0)/Value0) )

# ==== print (important stuff) ====
cat(paste(
  "Code ran:\n", 
  paste0( (clist2/clist)*100," % of original countries kept under rule.\n"),
  paste("Full countries:", clist),"\n",  
  paste("Used countries:", clist2))
)

