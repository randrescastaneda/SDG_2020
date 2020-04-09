# ==================================================
# project:       P90 p10 and p50
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-04
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             chart
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
# library("plotly")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
load("data/dfc.RData")
load("data/dfr.RData")

#----------------------------------------------------------
#   filtered dfc data
#----------------------------------------------------------

lyear <- 2015


dfc_1 <- dfc %>%
  filter(status == "OK") %>%
  pivot_wider(
    values_from = threshold,
    names_from = goal,
    names_prefix = "p",
    id_cols = c(countrycode, year)
  ) %>%
  filter(year == lyear,
         !is.na(p50),
         !is.na(p90),
         !is.na(p10)) %>%
  # merge regions and country names
  left_join(cr, by = "countrycode") %>%
  arrange(p50) %>%
  mutate(fcountrycode = factor(countrycode, countrycode),
         r9010       = p90/p10)

#----------------------------------------------------------
#   Gini data
#----------------------------------------------------------

df_g <- povcalnet(fill_gaps = TRUE) %>%   # Load povcalnet data
  filter(year == lyear) %>%

  # make we only have one observation per country
  group_by(countrycode) %>%
  mutate(n = sequence(n())) %>%
  filter((n == 1) |
         (n == 3 & (coveragetype  %in% c("N", "A"))) |
         (n == 2 & datatype == "consumptioin")) %>%

  # Keep important variables
  select(countrycode,
         year,
         gini,
         mean,
         population) %>%
  arrange(countrycode)

#
# dfc_1g <- dfc_1 %>%
#   left_join(df_g) %>%
#   mutate(countrycode = factor(countrycode, countrycode))


#----------------------------------------------------------
#   charts
#----------------------------------------------------------

# dfc_1 <-  dfc_1 %>%
#   arrange(r9010) %>%
#   mutate(fcountrycode = factor(countrycode, countrycode))



#--------- without Gini
p_p10p90 <- ggplot(data = dfc_1,
                   aes(x = fcountrycode,
                       y = p50)) +
  geom_point() +
  geom_errorbar(aes(ymin = p10,
                    ymax = p90,
                    color = region),
                width = .5) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5),
    legend.position = c(.2, .8),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(x = "Country code",
       y = "Dollar at day 2011 PPP")

# p_p10p90

dfc_ex <- dfc_1 %>%
  filter(countrycode == "CAN") %>%
  mutate(
    text = paste0(countryname, " (", round(r9010, digits = 1), ")")
  )


p_p10p90 +
  ggrepel::geom_label_repel(
    data = dfc_ex,
    aes(label = text,
        y = p90),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    # max.overlaps = 2,
    segment.curvature = 0.5,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_y = 5
  )



#--------- labels with 90/10 ratio

set.seed(1010)
sm_dfc <- c(1,sample(nrow(dfc_1), 20, prob = nrow(dfc_1)/dfc_1$r9010 ), nrow(dfc_1))
dfc_rep <- dfc_1[sm_dfc,] %>%
  mutate(
    text = paste0(countryname, " (", round(r9010, digits = 1), ")")
  )

p_p10p90l <- p_p10p90 +
  ggrepel::geom_label_repel(
    data = dfc_rep,
    aes(label = text),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    # max.overlaps = 2,
    segment.curvature = 0.5,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_y = 5
  )





# ggplotly(p_p10p90)


# ggplotly(p_p10p90, tooltip = "text")

#--------- with Gini
#
# adj_scale <- 150
# ggplot(data = dfc_1g) +
#   geom_point(aes(x = countrycode,
#                 y = p50)) +
#   geom_point(aes(x = countrycode,
#                  y = gini*adj_scale,
#                  color = region)) +
#   scale_y_continuous(sec.axis = sec_axis(~./adj_scale, name = "Gini Index")) +
#   geom_errorbar(aes(x = countrycode,
#                     ymin = p10,
#                     ymax = p90,
#                     color = region),
#                 width = 0.5) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(angle = 90,
#                                size = 5)
#   )
#


library(ggrepel)
p <- ggplot(mtcars,
            aes(wt, mpg, label = rownames(mtcars), colour = factor(cyl))) +
  geom_point()
p

p + geom_text_repel(nudge_x = ifelse(mtcars$cyl == 6, 1, 0),
                    nudge_y = ifelse(mtcars$cyl == 6, 8, 0))
