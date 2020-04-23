setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(scales)

# parameters
getWDI <- F
ordervariable <- "incomegroup"

if(getWDI == F){
  filename <- "GB40_new.png"
  WDI <- read.csv2("data/WDI2020.csv", sep = ",", dec = ".", stringsAsFactors = F)
  
  WDI <- WDI %>% 
    rename(Growth40=bottom_growth) %>% 
    rename(Growth=total_growth) %>%
    rename(GrowthMedian=median_growth) %>%
    rename(countrycode=code) %>% 
    mutate(region = ifelse(grepl("High Income", region),"OHI",as.character(region)))
  
  # -- Add Region ID
  cr <- read_rds("data/cty_regs_names.rds") %>%
    setDT()   %>% 
    select(-lending, -region)
  
  WDI <- left_join(WDI, cr, by = "countrycode")
  
} else{
  
  filename <- "GB40.png"
  
  # Download WDI data
  WDI <- wb(indicator =c("SI.SPR.PC40.ZG", 
                    "SI.SPR.PCAP.ZG"))
  
  WDI <- rename(WDI, countrycode=iso3c)
  
  # -- Add Region ID
  cr <- read_rds("data/cty_regs_names.rds") %>%
    setDT()
  
  cols  <- c("regioncode", "countryname")
  icols <- paste0("i.", c("region", "countryname", "incomegroup"))
  
  WDI <- merge(WDI, cr, by.x = "countrycode")
  
  # Reshape data
  WDI <- WDI %>% select(-indicator)
  WDI <- WDI %>% spread(indicatorID, value)  %>% 
    rename(Growth40=SI.SPR.PC40.ZG) %>% 
    rename(Growth=SI.SPR.PCAP.ZG)
}


# plots 

library(hrbrthemes)

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
  geom_vline(xintercept = 0, color = "red",  linetype="dashed")

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


## Additional plots only if using Shared prosperity data

data <- data %>%
  arrange(ordervar, Growth)
data$id <- seq(1, nrow(data))

data <- data %>% 
  mutate(GrowthMedian = as.numeric(as.character(GrowthMedian)))

# With median
pmedian <- ggplot(data) +
  geom_segment( aes(y=id, yend=id, x=Growth40, xend=Growth), color=rgb(0,0,0,0.3)) +
  geom_segment( aes(y=id, yend=id, x=Growth40, xend=GrowthMedian), color=rgb(0,0,0,0.3)) +
  geom_point( aes(y=id, x=Growth), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(y=id, x=Growth40), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  geom_point( aes(y=id, x=GrowthMedian), color=rgb(0.7,0.7,0.1,0.5), size=3 ) +
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
  geom_vline(xintercept = 0, color = "red",  linetype="dashed")

pmedian
# 
# # Ordering by mean income/consumption
# savepoint <- data
# 
# data <- data %>%
#   drop_na() %>% 
#   arrange(bottom__recent)
# data$id <- seq(1, nrow(data))
# 
# psort <- ggplot(data) +
#   geom_segment( aes(y=id, yend=id, x=Growth40, xend=Growth), color=rgb(0,0,0,0.3)) +
#   geom_point( aes(y=id, x=Growth), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
#   geom_point( aes(y=id, x=Growth40), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
#   coord_flip()+
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#   ) +
#   xlab("Growth") +
#   ylab("") +
#   geom_text(data=data, aes(y=id, x = -6, label = countrycode), angle = 90, alpha=0.6, size=4) +
#   geom_vline(xintercept = 0, color = "red",  linetype="dashed")
# 
# psort
# 
# 
# 
# 
# 
