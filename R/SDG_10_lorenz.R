# ==================================================
# project:       Lorenz curve of serveral countries
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-29
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             charts
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("haven")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")


#----------------------------------------------------------
#   Set up
#----------------------------------------------------------

pry <- "p:/01.PovcalNet/01.Vintage_control/PRY/PRY_2017_EPH/PRY_2017_EPH_V01_M_V01_A_GMD/Data/PRY_2017_EPH_V01_M_V01_A_GMD_GPWG.dta"

fin <- "p:/01.PovcalNet/01.Vintage_control/FIN/FIN_2015_EU-SILC/FIN_2015_EU-SILC_V01_M_V04_A_GMD/Data/FIN_2015_EU-SILC_V01_M_V04_A_GMD_GPWG.dta"

zaf <- "p:/01.PovcalNet/01.Vintage_control/ZAF/ZAF_2014_LCS/ZAF_2014_LCS_V02_M_V01_A_GMD/Data/ZAF_2014_LCS_V02_M_V01_A_GMD_GPWG.dta"



#----------------------------------------------------------
#   append data
#----------------------------------------------------------


nq   <- 10 # No. of quantiles
df <-  read_dta(pry) %>%
  select(countrycode, welfare, cpi2011, icp2011, weight) %>%
  bind_rows(
    read_dta(pry) %>%
        select(countrycode, welfare, cpi2011, icp2011, weight) %>%
      mutate(
        welfare = 1000,
        countrycode = "Equality"
      )
  ) %>%
  bind_rows(read_dta(fin) %>%
                select(countrycode, welfare, cpi2011, icp2011, weight)
            ) %>%
  bind_rows(read_dta(zaf)%>%
                select(countrycode, welfare, cpi2011, icp2011, weight)
            ) %>%
  mutate(
    y =  welfare/cpi2011/icp2011/365
  ) %>%
  filter(y >= 0) %>%
        select(countrycode, y, w = weight) %>%
  arrange(countrycode, y) %>%
  group_by(countrycode) %>%
  mutate(
    N  = sum(w, na.rm = TRUE),
    csw = cumsum(w),
    q  =  floor(csw/((N+1)/nq)) + 1,
    cy = cumsum(y*w)/max(cumsum(y*w), na.rm = TRUE),
    cw = cumsum(w)/max(cumsum(w), na.rm = TRUE),
    src = countrycode
  ) %>%
  ungroup() %>%
  select(-countrycode) %>%
  drop_na()

# ggplot(data = df,
#        aes(
#          x = y,
#          fill = src,
#          color = src,
#          weight = w
#        )
#        ) +
#   geom_density(alpha = 0.1) +
#   scale_x_continuous(
#     trans = "log",
#     labels = scales::number_format(accuracy = 0.01,
#                                    decimal.mark = ',')
#   ) +
#   # geom_vline(xintercept = omean) +
#   theme_classic()
#
# ggplot(data=df,
#        aes(x=cw,
#            y=cy,
#            color = src)) +
#   geom_line() +
#   geom_abline() +
#   scale_x_continuous(name="Cumulative share of population", limits=c(0,1)) +
#   scale_y_continuous(name="Cumulative share of welfare", limits=c(0,1)) +
#   theme_classic()


#
#
#
# ndf %>%
#   summarise(
#     gini = gini(y,w)
#   )
#
# ndf %>%
#   summarise(
#     mean = weighted.mean(y,w, na.rm = TRUE)
#   )

# set.seed(1010111)
# plt <- sample(palette, size = 4)
#
# breaks <- df %>%
#   group_by(src) %>%
#   summarise(
#     gini = gini(y,w)
#   ) %>%
#   arrange(-gini) %>%
#   pull(src)
#
# pe <- ggplot(data=filter(df, src %in% breaks[4]),
#              aes(x=cw,
#                  y=cy,
#                  color = src)) +
#   geom_line() +
#   scale_x_continuous(name="Cumulative share of population", limits=c(0,1)) +
#   scale_y_continuous(name="Cumulative share of welfare", limits=c(0,1)) +
#   theme_classic() +
#   scale_color_manual(values= plt,
#                      breaks = breaks)
# pe
#
