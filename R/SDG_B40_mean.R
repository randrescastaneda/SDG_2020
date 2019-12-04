# ==================================================
# project:       B40 and mean
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2019-12-02
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
library("plotly")
library("povcalnetR")


#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------




#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
source("R/_aux_data.R")

#----------------------------------------------------------
# Get data from Povcalnet
#----------------------------------------------------------

mdf <- povcalnet(fill_gaps = TRUE) %>%   # Load povcalnet data
  # Keep important variables
  select(contains("country"),
         contains("year"),
         starts_with("decil"),
         gini,
         mean,
         population) %>%
  # reshape long
  pivot_longer(cols = contains("decile"),
               names_to = "decile",
               values_to = "share") %>%

  # change formating of deciles
  mutate(
    decile = as.integer(str_replace(decile, "decile([1-9]+)", "\\1")),
  ) %>%  #mean of deciles
  filter(decile <= 4,
         !is.na(datayear)) %>%
  mutate(
    dm = (mean*share*10) # decile mean
  ) %>%
  group_by(countrycode,  year, datayear, countryname) %>%
  summarise_at(
    c("mean", "dm", "population", "gini"),
    mean, na.rm = TRUE
  )  %>%
  select(countrycode,
         countryname,
         year,
         b40m = dm,
         mean,
         gini) %>%
  # right_join(dfc50, by = c("countrycode", "year")) %>%  # median
  mutate(
    rat_b40m = b40m/mean
    # , rat_b40p50 = b40m/median   # median
  ) %>%

  # Get only one year per datayear
  mutate(diff_year = year - datayear) %>%
  group_by(countrycode) %>%
  filter(abs(diff_year) == min(abs(diff_year))) %>%
  group_by(countrycode, datayear) %>%
  mutate(n = n()) %>%
  filter((diff_year > 0 & n == 2) |
           n == 1) %>%
  select(-n, -diff_year) %>%
  ungroup() %>%
  # merge with region names dataframe
  left_join(rc) %>%
  arrange(countrycode, year)



#----------------------------------------------------------
#   Parameters
#----------------------------------------------------------




#----------------------------------------------------------
#   Charts
#----------------------------------------------------------

#--------- pallete

sw <- c("#34495e", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400")
scales::show_col(sw)

#--------- relationship between b40 share of mean and Gini

p_b40 <- ggplot(data = mdf,
        aes(x = gini, y = rat_b40m)) +
  geom_smooth(method = "lm", alpha = .4,
              color = "grey60") +
  geom_point(aes(color = region),
             alpha = .7) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    x = "Gini Coef.",
    y = "Share of B40 in overall mean"
  ) +
  theme_classic()  +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(linetype = c("blank"))
  )
p_b40

ggplotly(p_b40)

#--------- Lollipop plot of B40m as share of mean


df_b40_chg <- mdf %>%  # data frame of b40 change over time
  filter(year >= 2000)  %>%  # data after 2000
  group_by(countrycode) %>%
  filter((year == min(year)) |
          (year == max(year)) ) %>%
  arrange(countrycode, year) %>%
  mutate(n  = sequence(n())) %>%
  pivot_wider(names_from = n,
              values_from = c(rat_b40m, year),
              id_cols = countrycode) %>%
  left_join(cnames) %>%
  drop_na() %>%
  mutate(
    text = paste0("County: ", countryname, "\n",
                  "Range: ", year_1, "-", year_2, "\n")
  ) %>%
  # Reorder data using average
  rowwise() %>%
  mutate(mymean = mean(c(rat_b40m_1, rat_b40m_2)),
         premium = (rat_b40m_2 - rat_b40m_1) > 0) %>%
  ungroup() %>%
  arrange(premium, rat_b40m_2, rat_b40m_1) %>%
  mutate(countryname = factor(countryname, countryname))


# plot lollipop


ggplot(df_b40_chg) +
  geom_segment(aes(
    x    = countryname,
    xend = countryname,
    y    = rat_b40m_1,
    yend = rat_b40m_2
  ), color = "grey") +
  geom_point(aes(x = countryname,
                 y = rat_b40m_1),
             color = "#3498db",
             size = 3,
             alpha = .8) +
  geom_point(aes(x = countryname,
                 y = rat_b40m_2),
             color = "#f39c12",
             size = 3,
             alpha = .8) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",) +
  xlab("") +
  ylab("B40 as share of mean")

