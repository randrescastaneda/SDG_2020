# ==================================================
# project:       Relationship between the mean of different
#                 sections of the disitribution and the Gini
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-02
# Modification Date:
# Script version:    01
# References:
#
#
# Output:           charts and regressions
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("broom")
library("caret")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
decile_mean <- function(x, mdf) {
  # name <- quo_name(paste0("b", x,"0m"))
  df <- mdf %>%
    #mean of deciles
    filter(decile <= x,
           !is.na(datayear)) %>%
    group_by(countrycode,  year, datayear) %>%
    summarise(
      # !!name := mean(dm, na.rm = TRUE)
      bdm = mean(dm, na.rm = TRUE)
    ) %>%
    mutate(
      decile = x
    )
  return(df)
}
# d <- unique(mdf$decile)
# ndf <- map_df(d, decile_mean, mdf) %>%
#   arrange(countrycode,  year, decile, datayear)


#----------------------------------------------------------
#   Set up
#----------------------------------------------------------


mdf <- povcalnet(fill_gaps = TRUE)  %>%   # Load povcalnet data
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
  ) %>%
  mutate(
    dm = (mean*share*10) # decile mean
  ) %>%
  inner_join(
    map_df(unique(.$decile), decile_mean, .) %>%
      arrange(countrycode,  year, decile, datayear)
  ) %>%
  select(countrycode,
         countryname,
         year,
         datayear,
         decile,
         dm,
         bdm,
         mean,
         gini) %>%
  # right_join(dfc50, by = c("countrycode", "year")) %>%  # median
  mutate(
    rbdm = bdm/mean,
    rdm  = dm/mean,
    diff_year = year - datayear
  ) %>%

  # Get only one year per datayear
  group_by(countrycode) %>%
  filter(abs(diff_year) == min(abs(diff_year))) %>%
  group_by(countrycode, year, decile) %>%
  mutate(n = n()) %>%
  filter((diff_year > 0 & n == 2) |
           n == 1) %>%
  select(-n, -diff_year) %>%
  ungroup() %>%
  # merge with region names dataframe
  left_join(rc) %>%
  mutate(
    varid = paste0(countrycode, "-",year, "-", decile)
  ) %>%
  column_to_rownames(var = "varid") %>%
  arrange(countrycode, year, decile)


#----------------------------------------------------------
#   Charts
#----------------------------------------------------------

#--------- pallete
sw <- c("#34495e", "#3498db", "#2ecc71", "#f1c40f", "#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400")

sw <- c("#3969AC", "#F2B701", "#A5AA99", "#E68310", "#7F3C8D", "#11A579", "#E73F74", "#80BA5A", "#008695", "#CF1C90", "#f97b72", "#4b4b8f")

scales::show_col(sw)

#--------- relationship between b40 share of mean and Gini

p_bd <- ggplot(data = filter(mdf, decile  %in% c(1:9)),
              aes(x = rbdm,
                  y = gini,
                  color = as.factor(decile),
                  fill  = as.factor(decile)
                  )
               ) +
  geom_smooth(method = "lm",
              alpha = .4,
              color = "grey80") +
  geom_point(alpha = .7) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  labs(
    y = "Gini Coef.",
    x = "Share of B40 in overall mean"
  ) +
  theme_classic()  +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(linetype = c("blank"))
  ) +
  scale_color_manual(values = sw)
p_bd

ggplotly(p_bd)


#----------------------------------------------------------
# Regression
#----------------------------------------------------------

lm <- mdf %>%
  nest(data = -decile) %>%                                   # Split in several dataframes
  mutate(lm        = map(data, ~lm(gini ~rbdm,  data = .)),  # regression
         beta      = map(lm, ~tidy(.)[["estimate"]][2]),     # extract beta
         results   = map(lm, glance),                        # add beta
         fit       = map(lm, augment, se_fit = TRUE)         # add beta
  )

lm_res <- lm  %>%
  unnest(results) %>%
  select(-c(data, lm, beta, fit))

lm_fit <- lm  %>%
  unnest(fit) %>%
  select(-c(data, lm, beta, results))

#--------- loss function
lm_fit %>%
  group_by(decile) %>%
  summarise(
    mse = (sum(gini-.fitted)^2)/length(gini),   # Mean squared error
    mae =  sum(abs(gini-.fitted))/length(gini)  # Mean absolute error
  ) %>%
  left_join(
    lm_res %>%
    select(decile, r.squared, adj.r.squared)
  )


#----------------------------------------------------------
# cross validate
#----------------------------------------------------------
# control
ctrl <- trainControl(method = "cv",
                     number = 5)

cvm <- mdf %>%
  nest(data = -decile) %>%                                   # Split in several dataframes
  mutate(cvm = map(data, ~train(gini ~ rbdm,
                               data = .,
                               method = "lm",
                               trControl = ctrl
                               )
                   )
  )

cv <- train(gini ~ rbdm,
      data = mdf,
      method = "lm",
      trControl = ctrl
)



,   # cross validation
beta      = map(lm, ~tidy(.)[["estimate"]][2]),     # extract beta
results   = map(lm, glance),                        # add beta
fit       = map(lm, augment, se_fit = TRUE)         # add beta

#----------------------------------------------------------
#
#----------------------------------------------------------

