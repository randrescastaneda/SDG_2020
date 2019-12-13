# ==================================================
# project:       SDG 1 charts
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-11-11
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             Viz
# ==================================================

#----------------------------------------------------------
#   Run data
#----------------------------------------------------------

#source("R/SDG_01_data.R")

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("plotly")
library("paletteer")
library("ggrepel")


#----------------------------------------------------------
#   charts
#----------------------------------------------------------

#--------- prepare theme

# scales::show_col(new_swatch) # show colors scales
# scales::show_col(paletteer_d(package = "ggthemes", palette = "Tableau 20"))
# scales::show_col(paletteer_c(package = "ggthemes", palette = "Temperature"))
# palettes_d_names %>% filter(package == "ggthemes", length > 10)
# scales::show_col(swatch())

# gradient of line

gr_pl <- paletteer_dynamic(package = "cartography", palette = "blue.pal",
                           n = 12, direction = -1)

gr_pl <- paletteer_c(package = "ggthemes",
                     palette = "Classic Orange-White-Blue",
                     n = 26,
                     direction = -1)
#gr_pl <- gr_pl[3:length(gr_pl)]  # remove darkest colors

sw <- c("#34495e", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400")
clr_point <- sw[c(3, 5, 4, 6, 8, 9)]


xbreaks <- seq(0, 0.7, 0.1)

#--------- plot

plain <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  #axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  # legend.position = "bottom",
  legend.position = "none",
  legend.box = "horizontal"
)



# Number of poor
p1 <- ggplot(data = wld,
             aes(x = year,
                 y = poor_pop)) +
  geom_line(size = 1.5,
            color = "#3498db") +
  geom_label_repel(aes(label = ifelse(poor_pop  %in% c(max(wld$poor_pop),min(wld$poor_pop)),
                               paste(prettyNum(poor_pop, big.mark = ","),
                                     "Million"),
                               "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(500, max(wld$poor_pop))
  ) +
  labs(y = "Millions of people",
       x = "") +
  plain


# Global poverty trend
p2 <- ggplot(data = wld,
             aes(x = year,
                 y = headcount)) +
  geom_line(size = 1.5,
            color = "#34495e") +
  geom_label_repel(aes(label = ifelse(headcount  %in% c(max(wld$headcount),min(wld$headcount)),
                                      paste0(prettyNum(headcount*100, digits = 3),
                                            "%"),
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, max(wld$headcount))
  ) +
  geom_hline(yintercept = 0.03,
             linetype = "dashed",
             color = "#e74c3c",
             size = 1.2) +
  labs(y = "Poverty rate (%)",
       x = "") + plain


# Global poverty trend and goal

wld_f <-  lm(headcount ~ year,
             data = wld)

yv <- tibble( year = c(2016:2022))

wld2 <- wld %>%
  select(year, headcount) %>%
  arrange(year) %>%
  bind_rows(yv)

# 3% percent goal
p2_2 <- ggplot(data = wld2,
             aes(x = year,
                 y = headcount)) +
  geom_line(size = 1.5,
            color = "#34495e") +
  stat_smooth(method = "lm",
              fullrange = TRUE,
              se = FALSE,
              color = "grey50") +
  geom_label_repel(aes(label = ifelse(headcount  %in% c(max(wld$headcount),
                                                        min(wld$headcount)),
                                      paste0(prettyNum(headcount*100, digits = 3),
                                             "%"),
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, max(wld$headcount))
  ) +
  geom_hline(yintercept = 0.03,
             linetype = "dashed",
             color = "#e74c3c",
             size = 1.2) +
  labs(y = "Poverty rate (%)",
       x = "") + plain


# Global poverty rescaled to the fit variability.

p2_3 <- ggplot(data = wld,
             aes(x = year,
                 y = headcount)) +
  geom_line(size = 1.5,
            color = "#34495e") +
  geom_label_repel(aes(label = ifelse(headcount  %in% c(max(wld$headcount),min(wld$headcount)),
                                      paste0(prettyNum(headcount*100, digits = 3),
                                             "%"),
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8)
  ) +
  labs(y = "Poverty rate (%)",
       x = "") + plain




# country level
p3 <- ggplot() +
  geom_point(data = cty,
             aes(x = year,
                 y = headcount),
             alpha = .6,
             color = "#e74c3c",
             size = 2) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = ""
       ) + plain


# country level by size
p4 <- ggplot() +
  geom_point(data = cty,
             aes(x = year,
                 y = headcount,
                 size = poor_pop),
             alpha = .6,
             color = "#e74c3c") +
  scale_size(range = c(1, 15)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain

# two countries stand up


cty2 <- cty %>%
  filter(countrycode  %in% c("IND", "CHN"))

p5 <- ggplot() +
  geom_point(data = cty2,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = regionf),
             alpha = .8,
             pch = 21) +
  scale_fill_manual(values = c( "#d35400", "#f1c40f")) +
  scale_size(range = c(1, 15)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  geom_label_repel(data = cty2,
                   aes(x = year,
                       y = headcount,
                       label = ifelse(year == min(cty2$year),
                                      countryname,
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain



# too many (small) countries still have a poverty rate higher than the World's headcount

cty3 <- cty %>%
  left_join(select(wld, year, whdc = headcount), by = "year") %>%
  filter(!(countrycode  %in% c("IND", "CHN")),
         headcount > whdc)

p6 <- ggplot() +
  geom_point(data = cty3,
             aes(x = year,
                 y = headcount,
                 size = poor_pop),
             alpha = .6,
             color = "#e74c3c") +
  scale_size(range = c(1, 4)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain

# most of them belong to Africa

cty4 <- cty3 %>%
  mutate(
    regafr = if_else(region == "SSA",
                     "Africa",
                     "Non-African")
  )


cty4 <- cty %>%
  mutate(
    regafr = if_else(region == "SSA",
                     "Africa",
                     "Non-African")
  ) %>%
  filter(!(countrycode  %in% c("IND", "CHN")))


p7 <- ggplot() +
  geom_point(data = cty4,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = regafr),
             alpha = .8,
             pch = 21) +
  scale_fill_manual(values = c("#e74c3c", "#3498db")) +
  scale_size(range = c(1, 4)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "") + plain +
  theme( legend.position = c(.10, .15),
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.background = element_blank()
  ) +
  guides(
    size = FALSE,
    fill = guide_legend(title = "",
                        nrow = 2,
                        byrow = TRUE)
  )

# Other regions are not doing that bad
#... LAC

cty5 <- cty %>%
  filter(region == "LAC")


p8 <- ggplot() +
  geom_point(data = cty5,
             aes(x = year,
                 y = headcount,
                 size = poor_pop),
             alpha = .8,
             color = "#f1c40f") +
  scale_size(range = c(1, 4)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "") + plain +
  theme( legend.position = c(.15, .15),
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.background = element_blank()
  ) +
  guides(
    size = FALSE,
    fill = guide_legend(title = "",
                        nrow = 2,
                        byrow = TRUE)
  )

#... or ECA

cty6 <- cty %>%
  filter(region == "ECA")
"3498db"

p9 <- ggplot() +
  geom_point(data = cty6,
             aes(x = year,
                 y = headcount,
                 size = poor_pop),
             alpha = .8,
             color = "3498db") +
  scale_size(range = c(1, 4)) +
  geom_line(data = wld,
            aes(x = year,
                y = headcount),
            size = 1.5,
            color = "#34495e") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "") + plain +
  theme( legend.position = c(.85, .85),
         legend.direction = "horizontal",
         legend.title = element_blank(),
         legend.background = element_blank()
  ) +
  guides(
    size = FALSE,
    fill = guide_legend(title = "",
                        nrow = 2,
                        byrow = TRUE)
  )


# original
p10 <- ggplot() +
  geom_point(data = cty,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = regionf),
             alpha = .7,
             pch = 21) +
  scale_fill_manual(values = clr_point) +
  scale_size(range = c(1, 15)) +
  geom_line(data = wld,
            aes(x = year,  y = headcount),
            size = 1.5) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain


# projections

pty <-  pr_wld_act %>%
  filter(year == 2030) %>%
  select(year, headcount)

p11 <- ggplot() +
  geom_point(data = cty,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = region),
             alpha = .7,
             pch = 21) +
  scale_fill_manual(values = clr_point) +
  geom_line(data = subset(pr_wld_act, year >= 2018),
            aes(x = year,
                y = headcount,
                colour  = scenario),
            size = 1,
            alpha = .7) +
  geom_line(data = subset(pr_wld_act, year <= 2018),
            aes(x = year,
                y = headcount),
            size = 1.5,
            colour  = "#34495e") +
  geom_label_repel(data = pty,
                   aes(x = year,
                       y = headcount,
                       label = ifelse(headcount == min(pty$headcount),
                                      paste0("Best scenario, ",
                                             prettyNum(headcount*100, digits = 3),
                                             "%"),
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_label_repel(data = pty,
                   aes(x = year,
                       y = headcount,
                       label = ifelse(headcount == max(pty$headcount),
                                      paste0("Worst scenario, ",
                                             prettyNum(headcount*100, digits = 3),
                                             "%"),
                                      "")),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_colour_manual(values = gr_pl,
                      aesthetics = c("colour")) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = xbreaks
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain

#wld_gp2 <- ggplotly(wld_p2, tooltip = cty$text)



#sw <- c("#34495e", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400")
# scales::show_col(sw)
