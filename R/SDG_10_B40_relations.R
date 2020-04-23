setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

# libraries data manipulation
library(tidyverse)

#libraries plottig
library(ggridges)
library(ggplot2)
library(magick)
library(grid)
library(patchwork)

# Source get_WDI function
source("R/get_WDI.R")
source("R/panel_WDI.R")

mainvar <- "SI.SPR.PC40.ZG"
sharevar <- "SI.SPR.PCAP.ZG"
othervar <-  c("SI.POV.GINI", "SE.COM.DURS", "NY.GNP.PCAP.KD.ZG", "SG.LAW.NODC.HR")

index <- c(mainvar,sharevar,othervar)

j = 0
for (i in index){
  get_WDI(i)  
  if (j > 0){
    WDI <- WDI %>% 
      select(countrycode, i)
  }
  if( j == 0){
    assign("data",WDI)
    data <- data %>% select(-usedy)
    labels <- codebook
  }
  else{
    data <- left_join(data,WDI, by="countrycode")
    labels <- rbind(labels, codebook)
  }
  j = j + 1
}

if(sharevar != ""){
 data$diff <- (data[,mainvar] - data[,sharevar])
}

data$main <- data[,mainvar]

# add variables change

panel_WDI(othervar,  start = 2005, end = 2015, maxdist = 3)

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
      filter(Year == 2015)
    
  }
}

gmain <- growth %>% select(countrycode, gother)

datag <- left_join(data, gmain, by = "countrycode")
  

# add gini change
# 
# panel_WDI("SI.POV.GINI", start = 2005, end = 2015, maxdist = 2)
# 
# gini <- WDI %>%
#   mutate(Year = paste0('Y', Year)) %>%
#   spread(Year,SI.POV.GINI) %>%
#   mutate(dgini = ((Y2015-Y2005)/Y2005)*100) %>%
#   select(countrycode, dgini)
# 
# data <- left_join(data, gini, by = "countrycode")


## --- Ploting --- ##

## Bottom 40 growth relative vs all and gini change
v <-  ggplot(datag, aes(x = factor(incomegroup, level= c ("Low income","Lower middle income",
                                                         "Upper middle income","High income")),
                       diff, fill=incomegroup)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  theme(
    panel.background = element_rect(fill = NA),
    legend.position="none",
    plot.title = element_text(size=11),
    panel.grid.major = element_line(colour = "grey90")
  ) +
  xlab("") +
  ylab("Growth Difference") +
  ggtitle("Growth income difference B40 by region")

vg <- ggplot(datag, aes(x = factor(incomegroup, level= c ("Low income","Lower middle income",
                                                         "Upper middle income","High income")),
                       gSI.POV.GINI, fill=incomegroup)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  xlab("") +
  ylab("Change in GINI") +
  theme(
    panel.background = element_rect(fill = NA),
    legend.position="none",
    plot.title = element_text(size=11),
    panel.grid.major = element_line(colour = "grey90")
  )
  
GiniPlot <- v + vg

GiniPlot

# discrimination laws and growth in the bottom 40 (Nothing here)

ld<-  ggplot(data, aes(x = factor(SG.LAW.NODC.HR), main)) +
  geom_boxplot() +
  theme(
    panel.background = element_rect(fill = NA),
    legend.position="none",
    plot.title = element_text(size=11),
    panel.grid.major = element_line(colour = "grey90")
  ) +
  xlab("") +
  ylab("Growth Difference") +
  ggtitle("Growth income difference B40 by region")

ld

## Seeking for relations 

k = 0
for (i in othervar){
  k = k + 1
  print(i)
  data$runvar <- data[,i]
  a <- labels %>% filter(indicatorID == i)
  a <- a[1,1]
  
 p <-  ggplot(data, aes(runvar, diff, color=incomegroup)) +
    geom_point() +
    theme_bw() +
    ggtitle(a)
 assign(paste0('p',k), p)
 
 q <-  ggplot(data, aes(runvar, main, color=incomegroup)) +
   geom_point() +
   theme_bw()+
   ggtitle(a)
 assign(paste0('q',k), q)
 
}

## plot by change ##

k = 0
for (i in gother){
  k = k + 1
  print(i)
  datag$runvar <- datag[,i]
  j <- substr(i, 2,nchar(i))
  a <- labels %>% filter(indicatorID == j)
  a <- a[1,1]
  
  pg <-  ggplot(datag, aes(runvar, diff, color=incomegroup)) +
    geom_point() +
    theme_bw() +
    ggtitle(paste("Growth",a))
  assign(paste0('pg',k), pg)
  
  qg <-  ggplot(datag, aes(runvar, main, color=incomegroup)) +
    geom_point() +
    theme_bw()+
    ggtitle(a)
  assign(paste0('qg',k), qg)
  
}



## By Gini change ##
# 
# # plot vs difference
# gini1 <-  ggplot(datag, aes(dgini, diff, color=incomegroup)) +
#   geom_point() +
#   theme_bw() +
#   ggtitle("Gini change vs Growth difference B40")
# 
# gini1
# 
# 
# # plot vs difference
# gini2 <-  ggplot(data, aes(dgini, main, color=incomegroup)) +
#   geom_point() +
#   theme_bw() +
#   ggtitle("Gini change vs Growth B40")
# 
# gini2



# 
# 
#   grob <- grobTree(textGrob(i, x=0.95,  y=0.5, hjust=0,
#                             gp=gpar(col="black", fontsize=17, fontface="bold")))
#   
#   data <-  get(paste0("WDI"))
#   p <- ggplot(data, aes(x = SI.SPR.PC40.ZG, y = incomegroup, fill = incomegroup, alpha = 0.3)) +
#     geom_density_ridges() +
#     theme_ridges() + 
#     theme(legend.position = "none")
#   
#       xlim(0, 1) +annotation_custom(grob)
#   
#   assign(paste0("p",i), p)
#   
#   
#   ggsave(filename = paste0(i,".png"),
#          plot = p,
#          device='png',
#          height = 250, width = 300, dpi = 300,
#          limitsize = F, units = "mm")
#   
#   a <- image_read(paste0(i,".png")) 
#   assign(paste0("img",i),a)
# 
# 
# 
# image_resize(c(img1990, img2002, img2015, img2015, img2015), '800x800!') %>%
#   image_background('white') %>%
#   image_morph() %>%
#   image_animate(optimize = TRUE, fps = 10)
