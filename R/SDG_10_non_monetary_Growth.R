# Growth Incidence Curve social programs 

#setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(hrbrthemes)
library(ggplot2)
library(treemap)
library(treemapify)
library(plotly)
library(RColorBrewer)
library(ggthemes)
library(scales)
library(ggpubr)

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

# Which indicators do you need?
indList <- c("per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot",
             "per_sa_allsa.ben_q1_tot",
             "IQ.CPA.PROT.XQ",
             "SL.EMP.VULN.ZS",
             "SL.EMP.VULN.FE.ZS",
             "SL.EMP.VULN.MA.ZS",
             "IQ.CPA.SOCI.XQ",
             "DT.ODA.OATL.KD",
             "TM.TAX.MRCH.SM.AR.ZS",
             "DT.ODA.ALLD.KD")

# get data
panel_WDI(indList, start=year0, end = year1, maxdist=max_span, cb = TRUE, long = T)

# Calculate growth
WDI <- WDI %>% 
  mutate(countryname = if_else(countrycode=="COD", "Congo Dem", countryname)) %>% 
  arrange(indicatorID, countrycode, Year) %>% 
  group_by(indicatorID, countrycode) %>% 
  mutate( growth = (((value[2] / value[1])^(1/(usedey-usedsy)))-1) * 100 )  # Annualized growth
  # mutate( growth = ((value[2] - value[1])/value[1])*100 )              # Overall growth

# load region data
# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT()   %>% 
  select(-lending)


#### Target 10.4 - Social Protection: covergare per quintile ####

# Minor Cleanup
coverind <- c("per_sa_allsa.cov_q1_tot",
             "per_sa_allsa.cov_q2_tot",
             "per_sa_allsa.cov_q3_tot",
             "per_sa_allsa.cov_q4_tot",
             "per_sa_allsa.cov_q5_tot")

coverd <-  WDI %>%
  filter(indicatorID %in% coverind) %>% 
  drop_na() %>% 
  extract(indicatorID, into = "quintile", regex = "_(q[0-9])_", remove=F) %>% 
  filter(countrycode != "CMR") # Odd value in Cameron 


footnote <-  str_remove_all("Coverage as Percentage of population per quintile participating in cash transfers and last resort 
  programs, noncontributory social pensions, other cash transfers programs (child, family 
  and orphan allowances, birth and death grants, disability benefits, and other allowances), 
  conditional cash transfers, in-kind food transfers (food stamps and vouchers, food rations,
  supplementary feeding, and emergency food distribution), school feeding, other social
  assistance programs (housing allowances, scholarships, fee waivers, health subsidies, 
  and other social assistance) and public works programs (cash for work and food for work).
  Estimates include both direct and indirect beneficiaries. Average compound annual growth rate.", "\n")
 
# plot
SPcover <- ggplot(coverd,aes(countrycode, quintile) ) +
  geom_tile(aes(fill= growth)) +
  scale_fill_distiller(palette = "RdBu",  limits=c(-40, 40), breaks=seq(-40,40,by=10)) +
  labs(title = "Annual Growth in coverage of social safety net programs", 
  subtitle = "in quintile (% of population)",
  caption = str_wrap(footnote, width = 200) ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.caption.position =  "plot",
    plot.title.position = "plot"
  ) +
  xlab("")
  
SPcover

#### Target 10.4 - Social Protection: compare incidence in the lowest quintile ####

incd <- WDI%>% 
  filter(indicatorID == "per_sa_allsa.ben_q1_tot") %>% 
  mutate(Year=paste0("v", Year)) %>% 
  spread("Year", "value") %>%
  arrange(v2007) %>% 
  ungroup() %>% 
  mutate(id = row_number()) %>% 
  filter(countrycode != "CMR") # Odd value in Cameron 

footnote <-  str_remove_all("Benefit incidence of social safety net programs to poorest quintile shows the
                            percentage of total social safety net benefits received by the poorest 20% of 
                            the population.", "\n")

lm_fit <- lm(growth ~ v2007, data=incd)
# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(growth_pred = predict(lm_fit, incd), v2007=incd$v2007)


SPinc <- ggplot(incd, aes(x = v2007, y = growth)) +
  geom_point(aes( color = incomegroup)) +
  geom_line(color='blue',data = predicted_df, aes(x=v2007, y=growth_pred), alpha = 0.2, size = 1.4) +
  geom_text( aes(label = countrycode, color = incomegroup), hjust=1.5, vjust=0.7, size = 3, angle = 0, alpha = 0.8)+
  xlab("Initial incidence") +
  ylab("Annual Growth")+
  labs(title = "Annual Growth in incidence of social safety net programs", 
       subtitle = "Poorest quintile of the population",
       color = "Income Group",
       caption = str_wrap(footnote, width = 200)) +
  theme_minimal() +
  theme(
  plot.caption = element_text(hjust = 0),
  plot.caption.position =  "plot",
  plot.title.position = "plot"
  )

#### Target 10.4 - Social Protection: CPI social protection rating ####


footnote <- str_remove_all("Social protection and labor assess government policies in social
             protection and labor market regulations that reduce the risk of becoming poor,
             assist those who are poor to better manage further risks, and ensure a minimal
             level of welfare to all people. All criteria within each cluster receive equal weight,
                           and each cluster has a 25 percent weight in the overall score, which is 
                           obtained by averaging the average scores of the four clusters. For each of 
                           the 16 criteria countries are rated on a scale of 1 (low) to 6 (high). 
                           The scores depend on the level of performance in a given year assessed 
                           against the criteria, rather than on changes in performance compared 
                           with the previous year. The CPIA exercise is intended to capture the 
                           quality of a country's policies and institutional arrangements,
                           focusing on key elements that are within the country's control, 
                           rather than on outcomes (such as economic growth rates) that are 
                           influenced by events beyond the country's control.", "\n")

CPIss <- WDI %>% 
  filter(indicatorID == "IQ.CPA.PROT.XQ") %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(countrycode) %>%
  arrange(countrycode, Year) %>%
  mutate(Year=paste0("v", Year)) %>% 
  spread("Year", "value") %>% 
  mutate(change = v2017 - v2007)

SPcpi <- CPIss %>%
  group_by(countrycode) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(countryname, change)) %>% 
  ggplot() +
  geom_bar(aes(y = country, x=v2017), stat = "identity", alpha = 0.6 , fill= "navy") +
  geom_bar(aes(y = country, x=v2007), stat = "identity", alpha = 0.6 , fill= "red2") +
  scale_x_continuous(limits = c(0,7), expand = c(0,-1)) +
  ylab("") +
  xlab("Score") +
  labs(title = "CPIA social protection rating",
       subtitle = "(1=low to 6=high)",
       caption = str_wrap(footnote, width = 100)
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.caption.position =  "plot",
    plot.title.position = "plot"
  )

#### Target 10.c - Remitance Costs ####

# This data is not accesible throught the API (odd)

remit <- read.csv2("data/Remitances.csv", sep = ",", stringsAsFactors=FALSE) 

# clean remit data
remit <-  remit %>% 
  select_if(~sum(!is.na(.))>0) %>% 
  rename(countrycode=Country.Code,
         countryname=Country.Name,
         seriescode=Series.Code,
         seriesname=ï..Series.Name) %>% 
  filter(seriescode!="")


# Add Region ID
cr2 <- cr %>% 
  select(countrycode,region,incomegroup)
remit <- left_join(remit, cr2, by = "countrycode")

# keep description
seriesdes <- remit %>% 
  select(seriescode, seriesname) %>% 
  unique() %>% 
  filter(row_number()<3)

# cost from and to 
for (code in c("SI.RMT.COST.OB.ZS", "SI.RMT.COST.IB.ZS")){
  if (code == "SI.RMT.COST.OB.ZS") { dataname = "remit_from" }
  else {dataname = "remit_to"}
  
  remit_d <- remit %>% 
    filter(seriescode == code) %>% 
    select(-seriesname) %>%
    gather("year", "value", X2011:X2017) %>% 
    extract(year, into = "year", regex = "([0-9]+)", remove=F) %>% 
    filter(value != "") %>% 
    mutate(value = as.numeric(value), 
           year = as.numeric(year)) %>% 
    group_by(countrycode) %>% 
    arrange(countrycode, year) %>% 
    filter(row_number()==n()|row_number()==1) %>% 
    mutate(span = year[2] - year[1],
           growth = (((value[2] / value[1])^(1/span))-1) * 100)  
  
  assign(dataname, remit_d)
  rm(remit_d)
  
}


## plot current remitance cost 

# from 
max <- max(remit_from$value)

prf <-  remit_from %>%
  group_by(countrycode) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  mutate(countryname = fct_reorder(countryname, value)) %>% 
  ggplot(aes(x=countryname, y = value)) +
     geom_bar(stat = "identity", alpha = 0.8 , fill= "steelblue4" ) +
     geom_tile() +
     coord_flip() +
     xlab("") +
     ylab("% of percentage of the amount sent") +
     labs(title = "Cost of sending remittances from", 
          subtitle = "Cost as percentage of the amount sent for sending USD 200")+
     scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) + theme_tufte() +
  theme(
    axis.text = element_text(size = 8)
  )


   

# to 
 
max <- max(remit_to$value)
 
prt <-  remit_to %>%
  group_by(countrycode) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  mutate(countryname = fct_reorder(countryname, value)) %>% 
  ggplot(aes(x=countryname, y = value)) +
      geom_bar(stat = "identity", alpha = 0.8 , fill= "red3", ) +
      geom_tile() +
      coord_flip() +
      xlab("") +
      ylab("% of percentage of the amount sent") +
      labs(title = "Cost of sending remittances to", 
           subtitle = "Cost as percentage of the amount sent for sending USD 200")+
      scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) +
      theme_tufte() +
      theme(
        axis.text = element_text(size = 6)
      )




 
 
premit <- ggarrange(prf, prt, ncol = 2, nrow = 1)

## plot change remitance cost 

max <- max(remit_from$value)

prfg <-  remit_from %>%
  group_by(countrycode) %>% 
  filter(span >= 5 & row_number()==n()) %>% 
  ungroup() %>% 
  mutate(countryname = fct_reorder(countryname, growth)) %>% 
  ggplot(aes(x=countryname, y = growth)) +
      geom_bar(stat = "identity", alpha = 0.8 , fill= "steelblue4" ) +
      geom_tile() +
      coord_flip() +
      xlab("") +
      ylab("% Annual Cost Growth") +
      labs(title = "Compound annual cost growth of sending remittances from", 
           subtitle = "Cost as percentage of the amount sent for sending USD 200")+
      scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) + theme_tufte() +
  theme(
    axis.text = element_text(size = 8)
  )

prtg <-  remit_to %>%
  group_by(countrycode) %>% 
  filter(span >= 5 & row_number()==n()) %>% 
  ungroup() %>% 
  mutate(countryname = fct_reorder(countryname, growth)) %>% 
  ggplot(aes(x=countryname, y = growth)) +
      geom_bar(stat = "identity", alpha = 0.8 , fill= "red3" ) +
      geom_tile() +
      coord_flip() +
      xlab("") +
      ylab("% Annual Cost Growth") +
      labs(title = "Compound annual cost growth of sending remittances to", 
           subtitle = "Cost as percentage of the amount sent for sending USD 200")+
      scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) + theme_tufte() +
  theme(
        axis.text = element_text(size = 6)
      )


premitc <- ggarrange(prfg, prtg, ncol = 2, nrow = 1)




#### Target 10.3  - CPIA policies for social inclusion/equity ####

CPIinc <- WDI %>% 
  filter(indicatorID == "IQ.CPA.SOCI.XQ") %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(countrycode) %>%
  arrange(countrycode, Year) %>%
  mutate(Year=paste0("v", Year)) %>% 
  spread("Year", "value") %>% 
  mutate(change = v2017 - v2007)

pincCPI <- CPIinc %>%
  group_by(countrycode) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(countryname, change)) %>% 
  ggplot() +
  geom_bar(aes(x = country, y=v2017), stat = "identity", alpha = 0.6 , fill= "orange2") +
  geom_bar(aes(x = country, y=v2007), stat = "identity", alpha = 0.6 , fill= "blue4") +
  scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) + theme_tufte() +
  xlab("") +
  ylab("Score") +
  coord_flip()
  # theme(
  #   axis.text = element_text(angle=90)
  # )
  





#### Target 10.b.1 Total resource flows for development, by recipient and donorcountries and type of flow ####

AID <- WDI %>% 
  filter(indicatorID == "DT.ODA.ALLD.KD") %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(countrycode) %>%
  arrange(countrycode, Year) %>%
  mutate(Year=paste0("v", Year)) %>% 
  spread("Year", "value") %>% 
  mutate(change = v2017 - v2007)

# The goal cares for the porest nations, let's create separate dataset with the data for the lower income nations


pAID <- AID %>%
  filter(incomegroup != "High income") %>% 
  group_by(countrycode) %>% 
  filter(row_number()==n()) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(countryname, growth)) %>% 
  ggplot() +
  geom_bar(aes(x = country, y=growth, fill = incomegroup), stat = "identity", alpha = 0.6 ) +
  scale_fill_manual(values=c("#F8766D", "#00BA38", "#619CFF"))+
  # geom_bar(aes(x = country, y=v2007), stat = "identity", alpha = 0.6 , fill= "blue4") +
  geom_hline( yintercept =  mean(AID[AID$incomegroup=="Low income",]$growth, na.rm = T), color = "red") +
  geom_hline( yintercept =  mean(AID[AID$incomegroup=="Lower middle income",]$growth, na.rm = T), color = "darkgreen") +
  geom_hline( yintercept =  mean(AID[AID$incomegroup=="Upper middle income",]$growth, na.rm = T), color = "blue") +
  scale_y_continuous(expand = c(0,0), breaks = scales::pretty_breaks(n = 10)) + theme_tufte() +
  xlab("") +
  ylab("Score") +
  coord_flip()
# theme(
#   axis.text = element_text(angle=90)
# )



