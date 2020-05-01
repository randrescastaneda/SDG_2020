setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(scales)
library(hrbrthemes)
library(plotly)

# functions
source("R/panel_WDI.R")         # Wrapper to wb function from wbstat

# parameters
ordervariables <- "incomegroup" # how to group countries, works with region, incomegroup or both c("region","incomegroup")
sortvariable <- "Growth40"      # How to sort values within groups Growth40, Growth, diff, diffabs?
rankvariable <- "diff"     # How to rank countries: Growth40, Growth, diff, diffabs?
filename <- ""                  # root file name 


# ## Keep countries in older points
# list2017 <- c("CHN", "BEL", "ECU", "KAZ", "BTN", "KHM", "BOL", "BRA", "RUS",
#               "COL", "PER", "CHL", "URY", "MKD", "THA", "MDA", "GEO", "VNM", 
#               "PAN", "UKR", "IDN", "SLV", "UGA", "TZA", "NOR", "IRN", "PAK",
#               "TGO", "ROU", "POL", "CHE", "LKA", "PHL", "FIN", "ARG", "DOM",
#               "DEU", "BEL", "KGZ", "RWA", "NLD", "USA", "ALB", "GRB", "PRT",
#               "HUN", "LVA", "ISL")


# ------------------ code ---------------------#


# Load Share Prosperity Data
WDI <- read.csv2("data/WDI2020.csv", sep = ",", dec = ".", stringsAsFactors = F)

WDI <- WDI %>% 
  rename(Growth40=bottom_growth) %>% 
  rename(Growth=total_growth) %>%
  rename(GrowthMedian=median_growth) %>%
  rename(countrycode=code) %>% 
  mutate(region = ifelse(grepl("High Income", region),"OHI",as.character(region)),
         diff = (Growth40 - Growth) ,
         diffabs = abs(diff))

# Load the old WDI data 
WDI2017 <- read.csv2("data/WDI2017.csv", sep = ",", dec = ".", stringsAsFactors = F)

WDI2017 <- WDI2017 %>% 
  rename(countryname=Country.Name,
         countrycode=Country.Code,
         indicatorID=Indicator.Code,
         indicator=Indicator.Name,
         ) %>%
  gather("year", "value", X1960:X2016 ) %>% 
  extract(year, into = "Year", regex = "([0-9]+)") %>% 
  mutate(Year=as.numeric(as.character(Year)))

# I'll keep just those indicatores I care for
WDI2017 <- WDI2017 %>% 
  filter( indicatorID %in% c("SI.SPR.PC40.ZG","SI.SPR.PCAP.ZG") ) %>% 
  drop_na() %>% 
  select(-indicator, -countryname) %>% 
  spread(indicatorID, "value") %>% 
  rename(OLDGrowth40=SI.SPR.PC40.ZG,
         OLDGrowth=SI.SPR.PCAP.ZG,
         OLDyear=Year) %>% 
  mutate(OLDdiff = (OLDGrowth40 - OLDGrowth) ,
         OLDdiffabs = abs(OLDdiff))


#join data 

WDI <- full_join(WDI, WDI2017, by = "countrycode") %>% 
  select(-region)


# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()   %>% 
  select(-lending)

WDI <- left_join(WDI, cr, by = "countrycode") %>% 
  select(-country) %>% 
  rename(country=countryname) %>% 
  mutate(country=if_else(countrycode=="KHM", "Cambodia",country),
         region=if_else(countrycode=="KHM", "EAP", region),
         incomegroup=if_else(countrycode=="KHM", "Lower middle income", incomegroup))

#=============#
#   plots     #
#=============#

for(ordervariable in ordervariables){
  
  ### All availible data ###
  data <- WDI
  
  data <- data %>%
    rowwise() %>%
    mutate( mymean = mean(c(Growth40,Growth))) %>%
    mutate_(ordervar = ordervariable) %>%
    mutate_(sortvar = sortvariable) %>%
    arrange(ordervar, sortvar)
  
  #data$id <- seq(1, nrow(data))
  
  # empy space between regions
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$ordervar)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$ordervar <- rep(unique(data$ordervar), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(ordervar, sortvar)
  data$id <- seq(1, nrow(data))
  
  # set lines
  a <- data %>% group_by(ordervar) %>%
    summarise(max = max(id), mean = mean(id)) %>%
    ungroup() %>%
    mutate(val = max(max) + 2)
  
  data <- data %>%
    mutate( Growth40 = as.numeric(as.character(Growth40)),
            Growth = as.numeric(as.character(Growth))
    )
  
  breaks = seq(min(data$Growth40,na.rm = T),max(data$Growth40,na.rm = T), length.out = 5)
  
  # Plot
  p <- ggplot(data) +
    geom_segment( aes(y=id, yend=id, x=Growth40, xend=Growth), color=rgb(0,0,0,0.3)) +
    geom_point( aes(y=id, x=Growth, text = country), color="black", fill = "white", shape = 21, stroke = 0.7, size=2 ) +
    geom_point( aes(y=id, x=Growth40, text = country), color="white", fill = "red", shape = 21, size=2 ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
    ) +
    xlab("Growth") +
    ylab("") +
    geom_text(data=data, aes(y=id, x = -7, label = country), angle = 0, alpha=0.6, size=2) +
    geom_text(data=a, aes(y=mean, x = 8, label = ordervar), angle = 0, alpha=0.6, size=3) +
    geom_segment(data= a, aes(y=max, yend=max, x=-5, xend=10), color=rgb(0,0,0,0.3)) +
    geom_vline(xintercept = 0, color = "red",  linetype="dashed") +
    ggtitle("Growth Bottom 40 vs National Average - 2012-2017", subtitle = "All available") +
    labs(caption = paste("Bottom and Total growth as reported in the Global Database of Share Prosperity."))
  
  p
  
  p1 <- ggplotly(p)
  
  
  ### 2017 points ###
  
  # data <- WDI %>% 
  # filter(countrycode %in% list2017)

  temp <- sortvariable
  sortvariable <- paste0("OLD",sortvariable)
  
  data <- WDI %>% 
    filter(OLDdiff>0)
  
  data <- data %>%
    rowwise() %>%
    mutate_(ordervar = ordervariable) %>%
    mutate_(sortvar = sortvariable) %>%
    arrange(ordervar, sortvar)
  
  #data$id <- seq(1, nrow(data))
  
  # empy space between regions
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$ordervar)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$ordervar <- rep(unique(data$ordervar), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(ordervar, sortvar)
  data$id <- seq(1, nrow(data))
  
  # set lines
  a <- data %>% group_by(ordervar) %>%
    summarise(max = max(id), mean = mean(id)) %>%
    ungroup() %>%
    mutate(val = max(max) + 2)
  
  data <- data %>%
    mutate( Growth40 = as.numeric(as.character(Growth40)),
            Growth = as.numeric(as.character(Growth))
    )
  
  breaks = seq(min(data$Growth40,na.rm = T),max(data$Growth40,na.rm = T), length.out = 5)
  

  # Plot
  p <- ggplot(data) +
    geom_segment( aes(y=id, yend=id, x=Growth40, xend=Growth), color=rgb(0,0,0,0.3)) +
    geom_point( aes(y=id, x=Growth, text = country), color="black", fill = "white", shape = 21, stroke = 0.7, size=2) +
    geom_point( aes(y=id, x=Growth40, text = country), color="white", fill = "red", shape = 21, size=2 ) +
    geom_segment( aes(y=id, yend=id, x=OLDGrowth40, xend=OLDGrowth), color="steelblue4", alpha = 0.5) +
    geom_point( aes(y=id, x=OLDGrowth, text = country), color="steelblue4", fill = "white", shape = 21, stroke = 0.7, size=2, alpha = 0.5 ) +
    geom_point( aes(y=id, x=OLDGrowth40, text = country), color="white", fill = "steelblue4", shape = 21, size=2, alpha = 0.5 ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
    ) +
    xlab("Growth") +
    ylab("") +
    geom_text(data=data, aes(y=id, x = -7, label = country), angle = 0, alpha=0.6, size=2) +
    geom_text(data=a, aes(y=mean, x = 8, label = ordervar), angle = 0, alpha=0.6, size=3) +
    geom_segment(data= a, aes(y=max, yend=max, x=-5, xend=10), color=rgb(0,0,0,0.3)) +
    geom_vline(xintercept = 0, color = "red",  linetype="dashed") +
    ggtitle("Growth Bottom 40 vs National Average - 2012-2017", subtitle = "Old top countries") +
    labs(caption = paste("Bottom and Total growth as reported in the Global Database of Share Prosperity."))
  
  p
  
  p2 <- ggplotly(p)
  
  sortvariable <- temp
  
  ### actual new ranking ###
  
  data <- WDI %>% 
  filter(diff > 0)

  data <- data %>%
    rowwise() %>%
    mutate_(ordervar = ordervariable) %>%
    mutate_(sortvar = sortvariable) %>%
    arrange(ordervar, sortvar)
  
  #data$id <- seq(1, nrow(data))
  
  # empy space between regions
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$ordervar)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$ordervar <- rep(unique(data$ordervar), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(ordervar, sortvar)
  data$id <- seq(1, nrow(data))
  
  # set lines
  a <- data %>% group_by(ordervar) %>%
    summarise(max = max(id), mean = mean(id)) %>%
    ungroup() %>%
    mutate(val = max(max) + 2)
  
  data <- data %>%
    mutate( Growth40 = as.numeric(as.character(Growth40)),
            Growth = as.numeric(as.character(Growth))
    )
  
  breaks = seq(min(data$Growth40,na.rm = T),max(data$Growth40,na.rm = T), length.out = 5)
  

  # Plot
  p <- ggplot(data) +
    geom_segment( aes(y=id, yend=id, x=Growth40, xend=Growth), color=rgb(0,0,0,0.3)) +
    geom_point( aes(y=id, x=Growth, text = country), color="black", fill = "white", shape = 21, stroke = 0.7, size=2 ) +
    geom_point( aes(y=id, x=Growth40, text = country), color="white", fill = "red", shape = 21, size=2 ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
    ) +
    xlab("Growth") +
    ylab("") +
    geom_text(data=data, aes(y=id, x = -7, label = country), angle = 0, alpha=0.6, size=2) +
    geom_text(data=a, aes(y=mean, x = 8, label = ordervar), angle = 0, alpha=0.6, size=3) +
    geom_segment(data= a, aes(y=max, yend=max, x=-5, xend=10), color=rgb(0,0,0,0.3)) +
    geom_vline(xintercept = 0, color = "red",  linetype="dashed") +
    ggtitle("Growth Bottom 40 vs National Average - 2012-2017", subtitle = "New Ranking") +
    labs(caption = paste("Bottom and Total growth as reported in the Global Database of Share Prosperity."))
  
  
  p
  
  p3 <- ggplotly(p)
  

}

subplot(p1, p2 , p3)


## Bump chart 

rankvariableOLD <- paste0("OLD",rankvariable) 

rankd <- WDI %>%
  mutate_(rankvar=rankvariable,
          rankvarOLD=rankvariableOLD) %>% 
  filter(is.na(rankvar)==F,
         is.na(rankvarOLD)==F) %>% 
  ungroup() %>% 
  arrange(-rankvar) %>% 
  mutate(rankNEW = row_number()) %>% 
  arrange(-rankvarOLD) %>% 
  mutate(rankOLD = row_number(),
         rank1 = rankOLD,
         rank2 = rankNEW) %>%
  arrange(-rankOLD) %>% 
  mutate(id = as.factor(row_number())) %>% 
  gather("time","rank", rank1:rank2) %>% 
  mutate(time=if_else(time=="rank2","2012-2017","2008-2013")) %>% 
  mutate(diffstatus=if_else(diff>0,"Faster","Slower"),
         rankstatus=if_else(rankOLD<rankNEW,"Down Rank","Up Rank"))

# ranking plot 
pr <- ggplot(rankd) +
        geom_line(aes(y=id, x=rank, color = rankstatus)) +
        geom_point(aes(y = id, x = rankNEW, color = rankstatus, text = country)) +
        geom_point(aes(y = id, x = rankOLD, text = country)) +
        scale_x_continuous(breaks = 1:(nrow(rankd)/2))+
        theme_minimal() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 8),
        ) +
        ylab("") +
        xlab("Rank") +
        geom_text(aes(y=id, x = -3, label = country, angle = 0), alpha=0.6, size=3) 
  



plot_rank <- ggplotly(pr)

# Regular Bumpchart, not the best option 
bc <- ggplot(rankd) +
        geom_line(aes(x=time, y=rank, group = country, color=country), alpha=0.5) +
        geom_point(aes(x=time, y=rank, group = country, color=country), alpha=0.5) +
        scale_y_reverse(breaks = 1:nrow(rankd))

Bumpchart <- ggplotly(bc)

