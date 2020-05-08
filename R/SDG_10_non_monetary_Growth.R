# Growth Incidence Curve social programs 

setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(ggplot2)
library(treemap)
library(treemapify)
library(plotly)
library(RColorBrewer)

# Source get_WDI function
source("R/get_WDI.R")
source("R/panel_WDI.R")


# ================================== code start ==============================

# Parameters
year0 <- 2007      # Circa which years?
year1 <- 2017
max_span <- 4     # how far can an obs be from the reference year?
min_obs <- 10       # Minimum number of countries to make a plot 
dirsave <- "SDG_10_imgDV"

## Change in social protection coverege per quartile ##

# Which indicators do you need?
indList <- c("per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot",
             "per_sa_allsa.ben_q1_tot")

# get data
panel_WDI(indList, start=year0, end = year1, maxdist=max_span, cb = TRUE, long = T)

# Calculate growth
WDI <- WDI %>% 
  arrange(indicatorID, countrycode, Year) %>% 
  group_by(indicatorID, countrycode) %>% 
  mutate( growth = (((value[2] / value[1])^(1/(usedey-usedsy)))-1) * 100 )  # Annualized growth
  # mutate( growth = ((value[2] - value[1])/value[1])*100 )              # Overall growth


#### heatmap covergare per quartiles ####

# Minor Cleanup
coverind <- c("per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot")

coverd <-  WDI %>%
  filter(indicatorID %in% coverind) %>% 
  drop_na() %>% 
  extract(indicatorID, into = "quartile", regex = "_(q[0-9])_", remove=F) %>% 
  filter(countrycode != "CMR") # Odd value in Cameron 

heat <- ggplot(coverd,aes(countrycode, quartile) ) +
  geom_tile(aes(fill= growth)) +
  scale_fill_distiller(palette = "RdBu",  limits=c(-40, 40), breaks=seq(-40,40,by=10))
  
heat

#### compare incidence in the lowest quatile ####

incd <- WDI%>% 
  filter(indicatorID == "per_sa_allsa.ben_q1_tot") %>% 
  mutate(Year=paste0("v", Year)) %>% 
  spread("Year", "value") %>%
  arrange(v2007) %>% 
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  filter(countrycode != "CMR") # Odd value in Cameron 
  

ggplot(incd, aes(x = v2007, y = growth, size = v2007, color = incomegroup, label = countrycode)) +
  geom_point() +
  geom_text(hjust=1.5, vjust=0.7, size = 3, angle = 0, alpha = 0.8)+
  xlab("Initial incidence") +
  ylab("Annual Growth")
# +
#   theme(
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank()
#   ) 

