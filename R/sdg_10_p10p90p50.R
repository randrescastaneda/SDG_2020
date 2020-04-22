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
library("ggrepel")


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

p_p50 <- ggplot(data = dfc_1,
                   aes(x = fcountrycode,
                       y = p50)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5),
    legend.position = c(.2, .8),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(x = "Country code",
       y = "Dollar a day 2011 PPP")+
  ylim(0, max(dfc_1$p90))

set.seed(10123)
sm_p50 <- c(1,sample(nrow(dfc_1), 10 ), nrow(dfc_1))
dfc_p50 <- dfc_1[sm_p50,] %>%
  mutate(
    text = paste0(countryname, " ($", round(p50, digits = 2), " a day)")
  )

# Just first and last
dfc_p50lh <- dfc_p50[c(1,nrow(dfc_p50)),]


# p_p50 +
#   ggrepel::geom_label_repel(
#     data = dfc_p50,
#     aes(label = text,
#         fill  = region),
#     show.legend = FALSE,
#     force = 20,
#     box.padding = 1.2,
#     # max.overlaps = 2,
#     segment.curvature = 0.5,
#     # segment.ncp = 3,
#     # segment.angle = 20,
#     nudge_y = 5
#   )
#
# p_p50 +
#   ggrepel::geom_label_repel(
#     data = dfc_p50lh,
#     aes(label = text,
#         fill  = region),
#     show.legend = FALSE,
#     force = 20,
#     box.padding = 1.2,
#     # max.overlaps = 2,
#     segment.curvature = 0.5,
#     # segment.ncp = 3,
#     # segment.angle = 20,
#     nudge_y = 5
#   )
#


#--------- p90/p10
p_p10p90 <- p_p50 +
  geom_errorbar(aes(ymin = p10,
                    ymax = p90,
                    color = region),
                width = .5)


# p_p10p90

dfc_ex <- dfc_1 %>%
  filter(countrycode == "COL") %>%
  mutate(
    t90 = paste0(countryname, " 90th percentile ($", round(p90, digits = 1), " a day)"),
    t10 = paste0(countryname, " 10th percentile ($", round(p10, digits = 1), " a day)")
  )


p_p10p90lc <- p_p10p90 +
  ggrepel::geom_label_repel(
    data = dfc_ex,
    aes(label = t90,
        y = p90),
    show.legend = FALSE,
    force = 20,
    # box.padding = 5,
    segment.ncp = 2,
    segment.angle = 20,
    segment.curvature = 0.5,
    nudge_y = 10,
    nudge_x = -10,
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  ggrepel::geom_label_repel(
    data = dfc_ex,
    aes(label = t10,
        y = p10),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    segment.curvature = 0.5,
    nudge_y = -2,
    arrow = arrow(length = unit(0.01, "npc"))
  )


#----------------------------------------------------------
#   just two countries
#----------------------------------------------------------

dfc_2c <- dfc_1[c(1,nrow(dfc_1)),] %>%
  mutate(
    countryx = c("Country A", "Country B")
  ) %>%
  left_join(
    povcalnet(country = .[["countrycode"]],
              year    = unique(.[["year"]]),
              fill_gaps = TRUE) %>%
      select(countrycode, gini),
    by = "countrycode"
  )



p_p10p90_2cx <- ggplot(data = dfc_2c,
                   aes(x = countryx,
                      y = p50)) +
  geom_errorbar(aes(ymin = p10,
                    ymax = p90,
                    color = countryx),
                width = 1,
                size = 1.5) +
  geom_point(size = 2.5) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5),
    legend.position = c(.2, .8),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(x = "Country code",
       y = "Dollar a day 2011 PPP")+
  ylim(0, max(dfc_2c$p90))

# p_p10p90_2cx + geom_text(aes(label = paste("Gini:\n", round(gini, digits = 3))),
#                          nudge_x = -.1,
#                          nudge_y = 2.8)
#




# p_p10p90_2c


#----------------------------------------------------------
#   labels with 90/10 ratio
#----------------------------------------------------------


set.seed(1010)
sm_dfc <- c(1,sample(nrow(dfc_1), 20, prob = nrow(dfc_1)/dfc_1$r9010 ), nrow(dfc_1))
dfc_rep <- dfc_1[sm_dfc,] %>%
  mutate(
    text = paste0(countryname, " (", round(r9010, digits = 1), ")"),
    col1 = if_else(row_number() == 1, "a", "b"),
    col2 = if_else(row_number()  %in% c(1, n()),  "a", "b")
  )

p_p10p90l <- p_p10p90 +
  geom_label_repel(
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



p_p10p902 <- p_p10p90 +
  geom_label_repel(
    data = dfc_rep,
    aes(label = text,
        fill = col1),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    # max.overlaps = 2,
    segment.curvature = 0.5,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_y = 5
  )

p_p10p903 <- p_p10p90 +
  geom_label_repel(
    data = dfc_rep,
    aes(label = text,
        fill = col2),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    # max.overlaps = 2,
    segment.curvature = 0.5,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_y = 5
  )



# plotting the rank
p_r9010_j <- ggplot(data = dfc_1,
       aes(x = fcountrycode,
           y = r9010)) +
  geom_point(aes(color = region)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5),
    legend.position = c(.8, .8),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(x = "Country code",
       y = "P90/P10 ratio")+
  ylim(0, max(dfc_1$r9010))



dfc_2 <- dfc_1 %>%
  arrange(-r9010) %>%
  mutate(fcountrycode = factor(countrycode, countrycode))


dfc_rep2 <- dfc_2[1,] %>%
  mutate(
    text = paste0(countryname, " (", round(r9010, digits = 1), ")"),
    col1 = "a"
  ) %>%
  bind_rows(dfc_rep)

p_r9010_o <- ggplot(data = dfc_2,
       aes(x = fcountrycode,
           y = r9010)) +
  geom_point(aes(color = region,
                 size = p50)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90,
                               size = 5),
    legend.position = c(.8, .8),
    legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(x = "Country code",
       y = "P90/P10 ratio")+
  ylim(0, max(dfc_1$r9010))+
  geom_label_repel(
    data =  dfc_rep2,
    aes(label = text,
        fill = col2),
    show.legend = FALSE,
    force = 20,
    box.padding = 1.2,
    # max.overlaps = 2,
    segment.curvature = 0.5,
    # segment.ncp = 3,
    # segment.angle = 20,
    nudge_y = 5
  )

# relationship between p9010 and Gini

dfc_9010_g <- dfc_1 %>%
  left_join(povcalnetR::povcalnet(fill_gaps = TRUE,
                                  year      = 2015) %>%
              select(countrycode, gini),
            by = "countrycode")

p_9010_g <- ggplot(data = filter(dfc_9010_g,
                                 countrycode != "ZAF",
                                 !is.na(gini)),
                   aes(x = r9010,
                       y = gini)
                   ) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2)) +
  geom_point(aes(color = region)) +
  theme_classic() +
  theme(
    legend.position = c(.7, .3),
    # legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(y = "Gini coef.",
       x = "90/10 ratio")

# p_9010_g


p_med_g <- ggplot(data = filter(dfc_9010_g,
                                countrycode != "ZAF",
                                !is.na(gini)),
                 aes(y = gini,
                     x = p50)) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2)) +
  geom_point(aes(color = region)) +
  theme_classic() +
  theme(
    legend.position = c(.7, .7),
    # legend.direction = "horizontal",
    legend.title = element_blank()
  )  +
  labs(y = "Gini coef.",
       x = "Median welfare")
# p_med_g





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

#
# library(ggrepel)
# p <- ggplot(mtcars,
#             aes(wt, mpg, label = rownames(mtcars), colour = factor(cyl))) +
#   geom_point()
# p
#
# p + geom_text_repel(nudge_x = ifelse(mtcars$cyl == 6, 1, 0),
#                     nudge_y = ifelse(mtcars$cyl == 6, 8, 0))
