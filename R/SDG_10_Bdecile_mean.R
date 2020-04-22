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

# library("tidyverse")
library("data.table")
library("tidymodels")
library("janitor")
# library("broom")
library("caret")
library("povcalnetR")

source("R/_aux_data.R")
#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------


#--------- extract mean of whatever the bottom is


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

#--------- linear model by decile
lm_decile <- function(df) {
  lm <- df %>%
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

  lm_loss <- lm_fit %>%
    group_by(decile) %>%
    summarise(
      mse = (sum(gini-.fitted)^2)/length(gini),   # Mean squared error
      mae =  sum(abs(gini-.fitted))/length(gini)  # Mean absolute error
    ) %>%
    left_join(
      lm_res %>%
        select(decile, r.squared, adj.r.squared)
    )

  r <- list(lm = lm,
       res = lm_res,
       fit = lm_fit,
       loss = lm_loss)

  return(r)
}

#--------- Split data in K sections

k_split <- function(df, k) {
  folds <- split(sample(nrow(df), nrow(df), replace=F), (1:k))
  # lapply(folds, function(idxs) df[idxs, ])
  r <- purrr::map(folds, ~df[.x, ])
  return(r)
}


#----------------------------------------------------------
#   Prepare Data
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
  arrange(countrycode, year, decile) %>%
  as_tibble()


#----------------------------------------------------------
#   Charts
#----------------------------------------------------------

#--------- pallete
sw <- c("#34495e", "#3498db", "#2ecc71",
        "#f1c40f", "#e74c3c", "#9b59b6",
        "#1abc9c", "#f39c12", "#d35400")

sw <- c("#3969AC", "#F2B701", "#A5AA99",
        "#E68310", "#7F3C8D", "#11A579",
        "#E73F74", "#80BA5A", "#008695",
        "#CF1C90", "#f97b72", "#4b4b8f")

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
              color = "grey80",
              formula = y ~ x) +
  geom_point(alpha = .7) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  labs(
    y = "Gini Coef.",
    x = "Share of Bottom X in overall mean"
  ) +
  theme_classic()  +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(linetype = c("blank"))
  ) +
  scale_color_manual(values = sw)
p_bd

# ggplotly(p_bd)


#----------------------------------------------------------
# Regression
#----------------------------------------------------------

lm <- lm_decile(mdf)

print(lm$res)
print(lm$fit)
print(lm$loss)


#----------------------------------------------------------
# cross validate
#----------------------------------------------------------

# control

k <-  5
kmdf <- k_split(mdf, k)    # split data in 5

klm <- map(kmdf,lm_decile) # create lists, one for each model

#---- extract info

tbk <- tibble(klm=klm) %>%   # need to be in dataframe to unnest
  unnest_wider(klm)          # make it wide, where each list is a column

df_loss <- tbk$loss %>%
  map_df(rbind) %>%
  group_by(decile) %>%
  summarise_all(mean)

ggplot(data = df_loss,
       aes(x = decile,
           y = mae)
       ) +
  geom_point() +
  geom_line()  +
  scale_x_continuous(breaks = c(1:10))


#----------------------------------------------------------
# Cross validate
#----------------------------------------------------------
glimpse(mdf)

mdf %>%
  skimr::skim(gini, rbdm)

#--------- data splitting

# Fix the random numbers by setting the seed
set.seed(101110)
# Put 3/4 of the data into the training set
data_split <- initial_split(mdf, prop = 3/4)

# Create data frames for the two sets:
mdf_train <- training(data_split)
mdf_test  <- testing(data_split)


#--------- Create model

lr_mod <-
    linear_reg() %>%
    set_engine("lm") %>%
  set_mode("regression")

#--------- process

k <- 10
cv_mdf <- mdf %>%
  nest(data = -decile) %>%
  # Each 'variable' within mutate is dataframe
  mutate(

    # Split data
    data_split = map(data, ~initial_split(data = ., prop = 3/4)),
    mdf_train  = map(data_split, ~training(.)), # Training data
    mdf_test   = map(data_split, ~testing(.)),  # testing data

    # Create folds for cross validation using training data
    cv_folds   = map(mdf_train,  ~vfold_cv(., v = k)),

    # Linear model
    fit        = map(mdf_train,  ~fit(lr_mod, gini ~ rbdm, data = .)),

    # predicted values in testing data.
    yhat       = map2(fit, mdf_test,   ~predict(.x, new_data = .y)),
    yhat       = map2(yhat, mdf_test,  ~bind_cols(.x, .y)), # join covariates

    # Metrics for loss function
    rst        = map(cv_folds,   ~fit_resamples(model = lr_mod,
                                                gini ~ rbdm,
                                                metrics = metric_set(rmse, rsq, mae),
                                                resamples = .)),
    # summarise metrics
    cm = map(rst, ~collect_metrics(.))
    )

#--------- Results

cv_rslt <- cv_mdf %>%
  select(decile, cm) %>%
  unnest(cm)

#--------- plotting

ggplot(data = subset(cv_rslt, .metric != "rsq"),
       aes(x = decile,
           y = mean)
       ) +
  geom_point() +
  geom_line()  +
  scale_x_continuous(breaks = c(1:10)) +
  facet_grid(. ~ .metric)

#--------- testing data
cv_tst <- cv_mdf %>%
  select(decile, yhat) %>%
  unnest(yhat) %>%
  group_by(decile) %>%
  summarize(
    metricrmse = rmse_vec(truth = gini,
                    estimate = .pred),
    metricmae =  mae_vec(truth = gini,
                    estimate = .pred)
            ) %>%
  pivot_longer(cols         = starts_with("metric"),
               names_to     = "metric",
               names_prefix = "metric",
               values_to    = "value")


ggplot(data = subset(cv_tst, decile != 10),
       aes(x = decile,
           y = value)
) +
  geom_point() +
  geom_line()  +
  scale_x_continuous(breaks = c(1:10)) +
  facet_grid(. ~ metric)
