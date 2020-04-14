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

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")

#----------------------------------------------------------
# Gini data
#----------------------------------------------------------

# no interpolation

avg_gin_noint <- povcalnet(fill_gaps = TRUE) %>%
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



# Interpolating
minyear <- 1999
df_fg <- povcalnet(fill_gaps = TRUE) %>%
  left_join(select(cr, -countryname),
            by = "countrycode") %>%
  arrange(countrycode, coveragetype,datatype, year) %>%
  group_by(countrycode, coveragetype, datatype) %>%
  mutate(
    gini = zoo::na.approx(gini, year, na.rm = FALSE)
  )

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

#--------- Average and median Gini by region
avg_gini_reg <- df_fg %>%
  group_by(year, region) %>%
  summarise(ginisa  = mean(gini, na.rm = TRUE),
            ginimd  = median(gini, na.rm = TRUE),
            giniwa  = weighted.mean(gini, population, na.rm = TRUE)
            ) %>%
  pivot_longer(
    cols = -c(year, region),
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
  filter(!is.na(gini), year >= minyear) %>%
  group_by(countrycode, coveragetype, datatype) %>%
  filter(year  %in%  c(min(year), max(year))) %>%
  group_by(countrycode, coveragetype, datatype, year) %>%
  mutate(n  = n()) %>%
  filter((n == 1) |
         (n == 2 & datatype == "consumption")) %>%
  group_by(countrycode, coveragetype, datatype) %>%
  mutate(x  = sequence(n())) %>%
  pivot_wider(names_from = x,
              values_from = c(gini, year),
              id_cols = c(countrycode, coveragetype, datatype,
                          region, countryname, incomegroup, regionname)) %>%
  drop_na() %>%
  mutate(
    text = paste0("County: ", countryname, "\n",
                  "Range: ", year_1, "-", year_2, "\n",
                  "Region: ", region, "\n")
  ) %>%
  arrange(countrycode, coveragetype, year_2) %>%
  group_by(countrycode, coveragetype) %>%
  mutate(x  = sequence(n()),
         n  = n()) %>%
  filter(n == 1 | x == 2) %>%
  ungroup()

#----------------------------------------------------------
#   CHARTS
#----------------------------------------------------------

#--------- evolution of average and median gini NOT interpolating
p_av_g <- ggplot(data = filter(avg_gin_noint, type != "wa"),
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

#--------- evolution of average and median gini interpolating
p_av_g_int <- ggplot(data = filter(avg_gini, type != "wa"),
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

#--------- evolution of average and median gini in regions
p_av_gr <- ggplot(data = filter(avg_gini_reg, type == "sa"),
                 aes(
                   x = year,
                   y = gini,
                   color = region
                 )) +
  geom_point() +
  geom_line() +
  plain +
  theme(
    legend.title=element_blank(),
    legend.position = "bottom"
    )
# p_av_gr



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
       y = "Gini\n(circa 2018)") +
  scale_x_continuous(limits = c(min(min(df_g$gini_1), min(df_g$gini_2)), max(df_g$gini_1))) +
  scale_y_continuous(limits = c(min(min(df_g$gini_1), min(df_g$gini_2)), max(df_g$gini_2))) +
  theme_classic() +
  theme(
    legend.position = "",
  )

# p_g +
#   ggforce::geom_mark_hull(
#     aes(filter = region == 'LAC',
#         fill = region,
#         label = regionname),
#     show.legend = FALSE,
#     expand = unit(2, "mm"),
#     concavity = 1
#   )
#
# p_g +
#   ggforce::geom_mark_hull(
#     aes(filter = region == 'SSA',
#         fill = region,
#         label = regionname),
#     show.legend = FALSE,
#     expand = unit(2, "mm"),
#     concavity = 1
#   )
#
#
#
# p_g +
#   ggforce::geom_mark_hull(
#     aes(filter = region == 'EAP',
#         fill = region,
#         label = regionname),
#     show.legend = FALSE,
#     expand = unit(2, "mm"),
#     concavity = 1
#   )
#
#




# ggplotly(p_g, tooltip = "text")
