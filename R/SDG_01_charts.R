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

#-------------------- <- <- <- <- <- <- <- <- --------------------------------------
#   Run data
#----------------------------------------------------------

source("R/SDG_01_data.R")

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
# palettes_d_names %>% filter(package == "ggthemes", length > 10)
# scales::show_col(swatch())

# gradient of line
gr_pl <- paletteer_dynamic(package = "cartography", palette = "blue.pal",
                           n = 12, direction = -1)
gr_pl <- gr_pl[3:length(gr_pl)]  # remove darkest colors

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
    limits = c(0, max(wld$poor_pop))
    #breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
    #breaks = f_steps(10, zero = TRUE)
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
#sw <- c("#34495e", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400")
# scales::show_col(sw)

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
       x = "") +
  theme( panel.border = element_blank(),
         panel.grid = element_blank(),
         panel.background = element_rect(fill = "white"),
         plot.title = element_text(hjust = 0.5),
         legend.position = c(.15, .15),
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

# original
wld_p1 <- ggplot() +
  geom_point(data = cty,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = regionf),
             alpha = .7,
             pch = 21) +
  scale_fill_manual(values = clr_point) +
  geom_line(data = wld,
            aes(x = year,  y = headcount),
            size = 1.5) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain


# projections
wld_p2 <- ggplot() +
  geom_point(data = cty,
             aes(x = year, y = headcount,
                 size = poor_pop, fill = region),
             alpha = .7, pch = 21) +
  scale_fill_manual(values = clr_point) +
  geom_line(data = pr_wld_act,
            aes(x = year,  y = headcount, colour  = extragrowth),
            size = 1.5) +
  scale_colour_manual(values = gr_pl, aesthetics = c("colour")) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain

wld_gp2 <- ggplotly(wld_p2, tooltip = cty$text)



