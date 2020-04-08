# ==================================================
# project:       change in Gini from 2000
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-04
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
# library("plotly")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------




#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
# source("R/_aux_data.R")


#----------------------------------------------------------
# Gini data
#----------------------------------------------------------

minyear <- 1999
df_fg <- povcalnet(fill_gaps = TRUE)

#--------- Average and median Gini
avg_gini <- df_fg %>%
  group_by(year) %>%
  summarise(ginisa  = mean(gini, na.rm = TRUE),
            ginimd  = median(gini, na.rm = TRUE),
            giniwa  = weighted.mean(gini, population, na.rm = TRUE)
            ) %>%
  pivot_longer(
    cols = -year,
    names_to = c(".value", "type"),
    names_pattern = "(gini)(.*)"
  ) %>%
  mutate(
    type2 = case_when(
      type == "sa" ~ "Simple Avg.",
      type == "wa" ~ "Weighted Avg.",
      type == "md" ~ "Median",
      TRUE ~ ""
    )
  ) %>%
  ungroup() %>%
  filter(year >= 1990)

#-------------- min and max year
df_g <- df_fg %>%   # Load povcalnet data
  group_by(countrycode) %>%
  filter(year  %in%  c(minyear, max(year))) %>%
  group_by(countrycode, year) %>%
  mutate(n  = n()) %>%
  filter((n == 1) |
           (n == 3 & (coveragetype  %in% c("N", "A"))) |
           (n == 2 & datatype == "consumption")) %>%
  arrange(countrycode, year) %>%
  group_by(countrycode) %>%
  mutate(x  = sequence(n())) %>%
  pivot_wider(names_from = x,
              values_from = c(gini, year),
              id_cols = countrycode) %>%
  # merge regions and country names
  left_join(cnames) %>%  left_join(rc) %>%
  drop_na() %>%
  mutate(
    text = paste0("County: ", countryname, "\n",
                  "Range: ", year_1, "-", year_2, "\n",
                  "Region: ", region, "\n")
  ) %>%
  ungroup()

#----------------------------------------------------------
#   CHARTS
#----------------------------------------------------------

#--------- evolution of average and median gini
p_av_g <- ggplot(data = filter(avg_gini, type != "wa"),
                 aes(
                   x = year,
                   y = gini,
                   color = type2
                 )) +
  geom_point() +
  geom_line() +
  plain +
  theme(
    legend.title=element_blank(),
    legend.position = "bottom"
    )
# p_av_g


p_g <- ggplot(data = filter(df_g, gini_1 != gini_2),
              aes(x = gini_1,
                  y = gini_2,
                  color = region)) +
  geom_point(size = 2,
             show.legend = FALSE) +
  geom_abline(intercept = 0 ,
              slope = 1,
              color = "grey50") +
  labs(x = "Gini\n(circa 2000)",
       y = "Gini\n(circa 2015)") +
  scale_x_continuous(limits = c(.25, .6)) +
  scale_y_continuous(limits = c(.25, .6)) +
  theme_classic() +
  theme(
    legend.position = "",
  )

p_g +
  ggforce::geom_mark_hull(
    aes(filter = region == 'LAC',
        fill = region,
        label = region),
    show.legend = FALSE,
    expand = unit(2, "mm"),
    concavity = 1
  )


p_g +
  ggforce::geom_mark_hull()

# ggplotly(p_g, tooltip = "text")

