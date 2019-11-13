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
library("zoo")
library("plotly")
library("povcalnetR")
library("paletteer")
library("ggthemr")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#----------------------------------------------------------
#
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


# Global poverty
wld <- povcalnet_wb() %>%
  filter(year > 1989, regioncode == "WLD") %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  )

# Data at country level
cty <- povcalnet(fill_gaps = TRUE) %>%
  filter(year > 1989) %>%
  mutate(
    poor_pop = round(headcount * population, 0),
    headcount = round(headcount, 3)
  ) %>%
  inner_join(cr) %>%
  mutate(text = paste0("Country: ", countryname, "\n",
                       "Region: ", region, "\n",
                       "Headcount: ", round(headcount*100, digits = 1), "%\n",
                       "Million of poor: ", poor_pop, "\n",
                       "Year: ", year, "\n"))


headcount_col <- "#E69F00"
ggthemr('flat')


plain <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  #axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  # legend.position = "bottom",
  legend.position = "none",
  legend.box = "horizontal"
)



wld_p <- ggplot() +
  geom_line(data = wld,
            aes(x = year,  y = headcount),
            size = 1.5) +
  geom_point(data = wld,
            aes(x = year,  y = headcount)) +
  geom_point(data = cty,
             aes(x = year, y = headcount,
                 size = poor_pop, color = region),
             alpha = .7) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.8),
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
  ) +
  labs(y = "Poverty rate (%)",
       x = "",
       size = "Poor population\n(Millions)") + plain
wld_p


wld_gp <- ggplotly(wld_p)

wld_gp
