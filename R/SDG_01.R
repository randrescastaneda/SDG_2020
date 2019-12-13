# ==================================================
# project:       SDG 1 viz
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
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("plotly")
library("povcalnetR")
library("paletteer")
library("haven")
library("ggrepel")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#--------- define steps for breaking the axis of the chart

f_steps <- function(k, zero = TRUE) {
  step <- k
  if (zero == TRUE) {
    f <- 0
  } else {
    f <- floor(min(y))
  }

  function(y) {
    seq(f, ceiling(max(y)), by = step)
  }

}

#----------------------------------------------------------
# Prepare actual data
#----------------------------------------------------------

# countries and regions
regs <- c("EAP", "ECA", "LAC", "MNA", "SAS", "SSA")
reg <-  map(regs, get_countries)

cr <-  as_tibble(countrycode = NULL,
                 region = NULL) # country and regions


for (r in seq_along(regs)) {

  a <- tibble(countrycode = reg[[r]],
              region =  regs[r])

  cr <- bind_rows(cr, a)
}

st_year <- 1990

# Global poverty
wld <- povcalnet_wb() %>%
  filter(year > st_year, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  )

# Data at country level
cty <- povcalnet(fill_gaps = TRUE) %>%
  filter(year > st_year) %>%
  group_by(countrycode, year) %>%
  mutate(n  = n()) %>%
  filter((n == 1) |
           (n == 3 & (coveragetype  %in% c("N", "A"))) |
           (n == 2 & datatype == "consumption")) %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  ) %>%
  inner_join(cr) %>%
  mutate(text = paste0("Country: ", countryname, "\n",
                       "Region: ", region, "\n",
                       "Headcount: ", round(headcount*100, digits = 1), "%\n",
                       "Million of poor: ", poor_pop, "\n",
                       "Year: ", year, "\n"),
         regionf = as.factor(region))


#----------------------------------------------------------
#   Prepare forcasted data
#----------------------------------------------------------


#--------- Data at the country level

pr_cty <- read_dta("data/projections.dta") # load data provided by Daniel from Twinning

cutyr <- 2018
pr_cty <- pr_cty %>%
  filter(growth  %in% c("2018-2023", ""),    # Filter projection growth
         gic  %in% c("l", "")) %>%           # type of GIC
  select(-matches("^FGT[12]|^.*_3|^.*_55"))

#--------- Global data

# Collapse date by alpha, extragrowth and year
pr_wld <- pr_cty %>%
  group_by(alpha, extragrowth, year) %>%
  summarise(
    # weigthed mean by pop and divide by 100
    headcount = weighted.mean(x = FGT0_19, w = pop, na.rm = TRUE)/100
  ) %>%  ungroup() %>% arrange(year)

# get value of poverty for cutting year, (2018)
pr_temp <- pr_wld %>%
  filter(year == cutyr) %>%
  select(headcount) %>%
  pull()

# get combinations of alpha and extra growth from year 2019
pr_25 <- pr_wld %>%
  filter(year == cutyr + 1) %>%
  select(-headcount, -year) %>%
  mutate(
    headcount = pr_temp,   # add poverty year from 2018
    year = cutyr           # add year variable for cutyear == 2018
    )

#--------- joind actual data and projected data

pr_wld_act <- pr_wld %>%       # global projected
  filter(year > 2015) %>%      # stay with years after overlapping year (2015)
  bind_rows(pr_25) %>%         # append fake 2018 series
  bind_rows(wld) %>%           # append real data
  # Convert to factor and remove NA
  mutate(
    alpha = as.factor(ifelse(is.na(alpha), 0, alpha)),
    extragrowth = as.factor(ifelse(is.na(extragrowth), 0, extragrowth))
  )  %>%
  arrange(year) %>%
  filter(alpha == 0)     # projection filter... This has to change for the app

rm(pr_temp, pr_25)    # remove unnecessary data



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
p7 <- ggplot() +
  geom_point(data = cty3,
             aes(x = year,
                 y = headcount,
                 size = poor_pop,
                 fill = regionf),
             alpha = .6,
             pch = 21) +
  scale_fill_manual(values = clr_point) +
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



