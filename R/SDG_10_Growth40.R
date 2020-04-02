setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)

# Download WDI data
WDI <- wb(indicator =c("SI.SPR.PC40.ZG", 
                  "SI.SPR.PCAP.ZG"))

WDI <- rename(WDI, countrycode=iso3c)

# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()

cols  <- c("regioncode", "countryname")
icols <- paste0("i.", c("region", "countryname"))

WDI <- merge(WDI, cr, by.x = "countrycode")

# Reshape data
WDI <- WDI %>% select(-indicator)
WDI <- WDI %>% spread(indicatorID, value)  %>% 
  rename(Growth40=SI.SPR.PC40.ZG) %>% 
  rename(Growth=SI.SPR.PCAP.ZG)

# plots 

library(hrbrthemes)

# Reorder data
data <- WDI %>%
 rowwise() %>%
 mutate( mymean = mean(c(Growth40,Growth) )) %>%
 arrange(region, Growth)

#data$id <- seq(1, nrow(data))

# empy space between regions
empty_bar <- 1
to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$region)), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$region <- rep(unique(data$region), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(region, Growth)
data$id <- seq(1, nrow(data))

# set lines
a <- data %>% group_by(region) %>%
  summarise(max = max(id), mean = mean(id)) %>% 
  mutate(val = 10)

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
  geom_text(data=data, aes(y=id, x = -7, label = countrycode), angle = 90, alpha=0.6, size=3) +
  geom_text(data=a, aes(y=mean, x = 12, label = region), angle = 0, alpha=0.6, size=5) +
  geom_segment(data= a, aes(y=max, yend=max, x=-5, xend=10), color=rgb(0,0,0,0.3)) +
  geom_vline(xintercept = 0, color = "red",  linetype="dashed")

p



