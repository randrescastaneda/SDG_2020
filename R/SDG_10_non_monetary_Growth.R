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

# Source get_WDI function
source("R/get_WDI.R")
source("R/panel_WDI.R")


# ================================== code start ==============================

# Parameters
year0 <- 2005      # Circa which years?
year1 <- 2015
max_span <- 3     # how far can an obs be from the reference year?
min_obs <- 10       # Minimum number of countries to make a plot 
dirsave <- "SDG_10_imgDV"
# Which indicators do you need?
indList <- c("per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot")

# get data
panel_WDI(indList, start=year0, end = year1, maxdist=max_span, cb = TRUE, long = T)

WDI <- WDI %>% 
  arrange(indicatorID, countrycode, Year) %>% 
  group_by(indicatorID, countrycode) %>% 
  mutate( growth = (((value[2] / value[1])^(1/(usedey-usedsy)))-1) * 100 )  # Annualized growth
  # mutate( growth = ((value[2] - value[1])/value[1])*100 )              # Overall growth

# Minor Cleanup
WDI <-  WDI %>% 
  extract(indicatorID, into = "quartile", regex = "_(q[0-9])_", remove=F)

# # matrix form 
# WDImat <- WDI %>% 
#   ungroup() %>% 
#   filter(Year == year1) %>% 
#   select(countrycode, value, quartile) %>% 
#   spread( key = quartile, value) %>% 
#   remove_rownames() %>% 
#   column_to_rownames(var = "countrycode")
# 
# WDImat <- data.matrix(WDImat)
# WDImat <- t(WDImat)

# heatmap 
# a <- heatmap(WDImat, scale = "column", Colv = NA, Rowv = NA)

heat <- ggplot(WDI, aes(countrycode, quartile, fill= growth)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdYlBu")

heat



