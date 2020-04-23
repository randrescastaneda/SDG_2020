setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(scales)
library(hrbrthemes)

# functions
source("R/panel_WDI.R")

# parameters
ordervariables <- c("region","incomegroup")
circa <- 2017 
start <- 2012
end <- 2017
filename <- "GB40_new.png"

# Load Share Prosperity Data
WDI <- read.csv2("data/WDI2020.csv", sep = ",", dec = ".", stringsAsFactors = F)

WDI <- WDI %>% 
  rename(Growth40=bottom_growth) %>% 
  rename(Growth=total_growth) %>%
  rename(GrowthMedian=median_growth) %>%
  rename(countrycode=code) %>% 
  mutate(region = ifelse(grepl("High Income", region),"OHI",as.character(region)),
         diff = Growth40 - Growth )

# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()   %>% 
  select(-lending, -region)

WDI <- left_join(WDI, cr, by = "countrycode")

# plots 

for(ordervariable in ordervariables){
  # Reorder data
  
  data <- WDI
  
  data <- data %>%
    rowwise() %>%
    mutate( mymean = mean(c(Growth40,Growth))) %>% 
    mutate_(ordervar = ordervariable) %>%
    arrange(ordervar, Growth)
  
  #data$id <- seq(1, nrow(data))
  
  # empy space between regions
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$ordervar)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$ordervar <- rep(unique(data$ordervar), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(ordervar, Growth)
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
    geom_point( aes(y=id, x=Growth), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
    geom_point( aes(y=id, x=Growth40), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
    coord_flip()+
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
    ) +
    xlab("Growth") +
    ylab("") +
    geom_text(data=data, aes(y=id, x = -6, label = countrycode), angle = 90, alpha=0.6, size=4) +
    geom_text(data=a, aes(y=mean, x = 10.5, label = ordervar), angle = 0, alpha=0.6, size=8) +
    geom_segment(data= a, aes(y=max, yend=max, x=-5, xend=10), color=rgb(0,0,0,0.3)) +
    geom_vline(xintercept = 0, color = "red",  linetype="dashed") +
    labs(caption = paste("Bottom and Total growth as reported in the Global Database of Share Prosperity."))
  
  p
  
  # plot save
  if(ordervariable != "region"){
    filename <- paste0(ordervariable,"_",filename)
  }
  
  ggsave(filename = filename,
         plot = p,
         device='png',
         height = 300, width = 500, dpi = 300,
         limitsize = F, units = "mm")
}


## plot Gini relationship 

othervar <- "SI.POV.GINI"
data <- WDI 

# add GINI change

panel_WDI(othervar,  start = start, end = end, maxdist = 3)

growth <- WDI 
counter = 0
for (i in othervar){
  counter = counter + 1
  if(i %in% colnames(growth)){
    growth <- growth %>%
      group_by(countrycode) %>%
      arrange(countrycode, Year) %>% 
      mutate_(var = i) %>% 
      mutate(gwth = (var - lag(var))/ lag(var))
    
    growth[paste0("g",i)] <-  growth$gwth  
    if(counter == 1){
      gother <- c(paste0("g",i))
    }
    else{
      gother <- c(gother,paste0("g",i))
    }
  }
  else{
    print(paste("Not possible to create growth for", i))
  }
  if(length(othervar) == counter){
    growth <- growth %>% 
      select(-var, -gwth) %>% 
      filter(Year == end)
    
  }
}

gmain <- growth %>% select(countrycode, gother)

datag <- left_join(data, gmain, by = "countrycode")

k = 0
for (i in gother){
  k = k + 1
  print(i)
  plotname <- paste0("Growth_",i,".png")
  datag <- datag %>% 
    mutate_(runvar = i)
  
  j <- substr(i, 2,nchar(i))
  a <- codebook %>% filter(indicatorID == j)
  b <- a[1,2]
  a <- a[1,1]
  
  # create multiple linear model
  lm_fit <- lm(diff ~ runvar, data=datag)
  
  # save predictions of the model in the new data frame 
  # together with variable you want to plot against
  predicted_df <- data.frame(lm_pred = predict(lm_fit, datag), runvar=datag$runvar) 
  
  pg <-  ggplot(datag, aes(diff, runvar)) +
    geom_point(color='steelblue2', size = 3, alpha = 0.6) +
    geom_line(color='steelblue4',size = 1.2, alpha = 0.9, data = predicted_df, aes(x=lm_pred, y=runvar)) +
    theme_bw() +
    ylab(paste("Growth",a)) +
    xlab("Growth difference Total and Botttom 40") +
    labs(caption = paste("Growth circa 2012-2017. Bottom and Total growth as reported in the Global Database of Share Prosperity.
                         ",a ,"obatined from the WDI database; unique indicator id", b,"."))
    

  assign(paste0('pg',k), pg)
  
  ggsave(filename = plotname,
         plot = pg,
         device='png',
         height = 300, width = 500, dpi = 300,
         limitsize = F, units = "mm")
  
}


