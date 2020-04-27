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
bar_plot <- F                   # Do you want to add a variable as a barplot?
barvariable <- "diff"           # If you wnat bars, which is the variable to be plotted?

                                # --- To get other data from the WB databank
othervar <- "SI.POV.GINI"       # Any additional varible? 
# othervar <- "NY.GDP.PCAP.PP.CD"   # GDP pp (nothing here)
exactmatch <- T                 # Do you look for a exact match with Shared prosperity
                                # If not, select the circa year
circa <- 2017                   # Single points for which circa year?
start <- 2012                   # If growth measure, which start year? (circa)
end <- 2017                     # If growth measure, which end year? (circa)
filename <- "GB40_new.png"      # root file name 


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

# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()   %>% 
  select(-lending, -region)

WDI <- left_join(WDI, cr, by = "countrycode")

#=============#
#   plots     #
#=============#

#--- Main plot ----#

for(ordervariable in ordervariables){
  # Reorder data

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


  if(bar_plot == T){
    data <- data %>% mutate_(barvar = barvariable)
    p <- p + geom_bar(data = data, aes(x=barvar,y=id),stat="identity", alpha = 0.5)
  }

  # p

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



# ---- Gini relationship plot ----#

data <- WDI 

# add GINI change

if(exactmatch == T){

  periods <- unique(data$period)
  
  keepers <- data %>% 
    select(period, countrycode)

  counter = 0
  for(i in periods){
    counter = counter + 1
    values <- str_split(i, pattern = "-", simplify = T)
    panel_WDI(othervar,  start = as.numeric(values[1]), end = as.numeric(values[2]), maxdist = 1)
    
    WDI <- WDI %>% 
      mutate(period = i) %>% 
      inner_join(keepers, by=c("countrycode","period"))
    
    if(counter == 1){
      datum <- WDI 
      # datum <- inner_join(keepers, datum, by=c("countrycode","period"))
    } else {
      datum <- bind_rows(datum, WDI)
      # datum <- inner_join(keepers, datum, by=c("countrycode","period"))
    }
  }
  
  WDI <- datum %>% 
    separate(period, into=c("year0","year1"), remove = F) %>% 
    mutate(year0 = as.numeric(year0),
           year1 = as.numeric(year1))
  
} else {
  panel_WDI(othervar,  start = start, end = end, maxdist = 3)
  WDI <- WDI %>% 
    mutate(period = paste0(start,"-",end),
           year0 = start,
           year1 = end )
}

growth <- WDI 
counter = 0
for (i in othervar){
  counter = counter + 1
  if(i %in% colnames(growth)){
    growth <- growth %>%
      group_by(countrycode) %>%
      arrange(countrycode, Year) %>% 
      mutate_(var = i) %>% 
      mutate(gwth = (((var / lag(var))^(1/(year1 - year0))) - 1))
      #mutate(gwth = (var - lag(var))/ lag(var))
    
    growth[paste0("g",i)] <-  growth$gwth  
    if(counter == 1){
      gother <- c(paste0("g",i))
    } else{
      gother <- c(gother,paste0("g",i))
    }
  } else{
    print(paste("Not possible to create growth for", i))
  }
  if(length(othervar) == counter){
    growth <- growth %>% 
      select(-var, -gwth) %>% 
      filter(Year == year1)
    
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
  
  pg <-  ggplot(datag) +
    geom_point( aes(diff, runvar, text = countryname, color=regionname), size = 3, alpha = 0.6) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
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
  
  dpg <- ggplotly(pg)
  assign(paste0('pg',k), dpg)
  
}


