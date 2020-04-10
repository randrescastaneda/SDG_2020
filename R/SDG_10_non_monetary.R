setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(ggplot2)
library(treemap)
library(treemapify)

# Source get_WDI function
source("R/get_WDI.R")


# ================================== code start ==============================

# Parameters
circa <- 2015      # Circa which year?
max_span <- 5      # how far can an obs be from the reference year?
min_obs <- 10       # Minimum number of countries to make a plot 
dirsave <- "SDG_10_imgDV"
# Which indicators do you need?
indList <- c("per_allsp.cov_pop_tot",
             "per_sa_allsa.cov_pop_tot",
             "per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot",
             "SH.UHC.SRVS.CV.XD",
             "per_si_allsi.ben_q1_tot",
             "per_allsp.ben_q1_tot",
             "per_sa_allsa.ben_q1_tot",
             "per_lm_alllm.ben_q1_tot",
             "GC.XPN.COMP.ZS",
             "SH.MED.CMHW.P3",
             "SH.SGR.CRSK.ZS",
             "GC.XPN.TRFT.ZS",
             "SL.EMP.VULN.ZS",
             "SL.EMP.VULN.FE.ZS",
             "SL.EMP.VULN.MA.ZS",
             "SG.LAW.NODC.HR",
             "SG.NOD.CONS",
             "SI.RMT.COST.OB.ZS",
             "SI.RMT.COST.IB.ZS",
             "BX.TRF.PWKR.DT.GD.ZS")

# odd error not capture by the tryCatch with this one: "SI.RMT.COST.ZS", Not terrible we have a subtitute 

k <- 0
for (ind in indList){
  
  # short sleep to avoid problems w/ the API
  if (k == 10){
   Sys.sleep(8)
    k <- 0
  }
  else { k <- k + 1 }

  print(paste("Graphing", ind))
  
  skip <- F
  is_dummy <- F
  
  # get data 
  tryCatch(
    get_WDI(ind, circayr=circa, maxdist=max_span, cb = T),
    error=function(e){
      print(paste("Could not get",ind,"from the WB dataverse. Skipped."))
      skip<<-TRUE 
    }
  )
  
  if(skip) { next }
  
  # check we have enough data 
  if (nrow(WDI) < min_obs){
    print(paste(ind, "has to few observations on the circa year, skipped."))
    skip<<-TRUE
  }
  
  if(skip) { next } 
  
  data <- WDI %>% mutate(indv = get(ind))
  
  
  # check if indicator is a dummy 
  tryCatch(
    is_dummy <<- identical(c(as.numeric(levels(as.factor(data$indv)))), c(0,1), attrib.as.set = TRUE),
    error=function(e){
      print(paste("Could not test if",ind,"is dummy."))
    }
  )
  
  # rearrange data by value
  data <- data %>%
    rowwise() %>%
    arrange(indv)
  
  if (is_dummy == F){
  
  # empy space between regions
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*length(unique(data$region)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$region <- rep(unique(data$region), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(region, indv)
  data$id <- seq(1, nrow(data))
  
  # set lines
  separators <- data %>% group_by(region) %>%
    summarise(max = max(id), mean = mean(id)) %>% 
    mutate(val = 1)
  
  mean_world <- mean(data$indv, na.rm = T)
  
  # labels placing
  lbt <- max(data$indv, na.rm = T) + 0.5
  lbb <- min(data$indv, na.rm = T) - (sd(data$indv, na.rm = T)/3)
  
  
  # --- Plot
  
    # If not dummy
    p <- ggplot(data) +
      geom_segment( aes(y=id, yend=id, x=0, xend=indv), color=rgb(0,0,0,0.3)) +
      geom_point( aes(y=id, x=indv), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
      coord_flip()+
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
      ) +
      xlab("") +
      ylab("") +
      ggtitle(codebook[1,1]) +
      geom_text(data=data, aes(y=id, x = lbb, label = countrycode), angle = 90, alpha=0.6) +
      geom_text(data=separators, aes(y=mean, x = lbt, label = region), angle = 0, alpha=0.6) +
      geom_segment(data= separators, aes(y=max, yend=max, x=lbb, xend=lbt), color=rgb(0,0,1,0.2)) +
      geom_vline(xintercept = 0, color = "black",  linetype="dashed") +
      geom_vline(xintercept = mean_world, color = "red",  linetype="dashed")
  }
  else{
    # if dummy
    
    data <- data %>% mutate(value = 10)
    
    p <- ggplot(data, aes(area = value, fill = indv, 
                    label = countrycode, subgroup = region)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "black", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "white", place = "topleft", reflow = T)
  
  }
  
  ggsave(filename = paste0(ind,".png"), path = dirsave,
    plot = p,
    device='png',
    height = 200, width = 500, dpi = 300,
    limitsize = F, units = "mm")
  
  print("Plot saved :)")
  
}
