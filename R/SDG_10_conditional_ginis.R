# ==================================================
# project:       Analysis of Gini
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-04-07
# Modification Date:
# Script version:    01
# References:
#
#
# Output:
# ==================================================

#----------------------------------------------------------
#   Load libraries
#----------------------------------------------------------

library("tidyverse")
library("data.table")
library("janitor")
library("CGPfunctions")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------

gini <- function(x, y, w = NULL) {

  y <- deparse(substitute(y))
  w <- deparse(substitute(w))

  y    <- x[[y]]   # welfare

  if (w == "NULL") {
    w = rep(1, times = length(y))
  } else {
    w    <- x[[w]]    # weigth
  }

  ordy <- order(y)     # order of y

  w    <- w[ordy]      #order weight
  y    <- y[ordy]      # order welfare

  N    <- sum(w)       # population size
  Y    <- sum(y*w)     # total welfare

  cw   <- cumsum(w)    # Cumulative weights
  cy   <- cumsum(y*w)  # Cumulative welfare

  sn   <-  w/N         # share of population
  my   <- weighted.mean(y, w, na.rm = TRUE)

  i    <- (2*cw - w + 1)/2
  t2   <- y*(N - i + 1)
  gini <- 1+(1/N) - (2/(my*N^2))*sum(t2*w)
  return(gini)
}

palette <- c("#1D6996", "#EDAD08", "#0F8554", "#5F4690",
             "#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2",
             "#E17C05", "#CC503E", "#94346E", "#6F4070",
             "#855C75", "#D9AF6B", "#AF6458", "#736F4C",
             "#68855C", "#9C9C5E", "#A06177", "#8C785D",
             "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7",
             "#38A6A5", "#73AF48", "#87C55F", "#9EB9F3",
             "#526A83", "#625377", "#994E95", "#666666",
             "#467378", "#7C7C7C", "#D3B484", "#B3B3B3"
             )
scales::show_col(palette[1:12])

lc2 <- read_rds("data/cts_dist.rds")
cr  <- read_rds("data/cty_regs_names.rds")
#----------------------------------------------------------
#   calculations
#----------------------------------------------------------


g0100 <- lc2 %>%   # Gini total
  map_dbl(gini,welfare, weight) %>%
  tibble(g0100 = .,
         id = attr(., "names")
  )

g0095 <- lc2 %>%
  map(~.x[.x[["q"]] < 20,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0095 = .,
         id = attr(., "names")
  )

g0595 <- lc2 %>%
  map(~.x[.x[["q"]] > 1 & .x[["q"]] < 20,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0595 = .,
         id = attr(., "names")
  )


g0090 <- lc2 %>%
  map(~.x[.x[["q"]] < 19,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g0090 = .,
         id = attr(., "names")
  )


g1090 <- lc2 %>%
  map(~.x[.x[["q"]] > 2 & .x[["q"]] < 19,]) %>%
  map_dbl(gini,welfare, weight) %>%
  tibble(g1090 = .,
         id = attr(., "names")
  )

gt_w <- g0100 %>%
  inner_join(g0095)   %>%
  inner_join(g0595) %>%
  inner_join(g0090)   %>%
  inner_join(g1090) %>%
  select(id, everything()) %>%
  filter_if(is.numeric, all_vars(.>0 & . < 1)) %>%
  mutate(
    countrycode = gsub("([A-Z]+)(.*)", "\\1", id),
    year        = gsub("(\\D+)([0-9]+)(\\D+)", "\\2", id),
    coverage    = gsub("([A-Z]+)([0-9]+)([[:alpha:]])", "\\3", id)
  ) %>%
  left_join(cr, by = "countrycode")

#--------- remove unnecessary objects
rm(list=ls(pattern = "g\\d+"))
rm(lc2)

#----------------------------------------------------------
#   wrangling data
#----------------------------------------------------------


gt_l <- gt_w %>%
  pivot_longer(cols           = starts_with("g"),
               names_to       = "gini_type",
               names_prefix   = "g",
               values_to      = "value",
               values_drop_na = TRUE) %>%
  mutate(gini_type     = ordered(gini_type, levels = c("0100", "0095", "0595", "0090", "1090")),
         countrycode   = as.factor(countrycode),
         value         = round(value, digits = 3)) %>%
  group_by(countrycode, year, coverage) %>%
  mutate(
    rank = order(value)
  ) %>%
  ungroup()

xvar <- "g1090"
ggplot(data = gt_w,
       aes(x = get(xvar),
           y = g0100,
           color = region)
) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlim(.1, .7) + ylim(.1, .7)



gt_l <- gt_w %>%
  group_by(year) %>%

  # create ranking of gini (rg)
  mutate_at(
    vars(starts_with("g")), list(r = ~rank(.))
  ) %>%
  rename_at(
    vars(ends_with("_r")),
    list(~ paste("r", gsub("_r", "", .), sep = ""))
    ) %>%

  # create difference in ranking (drg)
  mutate_at(
    vars(starts_with("rg")), list(d = ~ abs(rg0100 -.))
  ) %>%
  rename_at(
    vars(ends_with("_d")),
    list(~ paste("d", gsub("_d", "", .), sep = ""))
  ) %>%

  # reshape to long format
  pivot_longer(
    -c(id, countrycode, year, coverage, countryname, region, regionname, incomegroup, lending),
    names_to      = c(".value", "gini_type"),
    names_pattern = "(d?r?g)([\\d]+)"
  ) %>%

  # change type to chart
  mutate(gini_type     = ordered(gini_type, levels = c("0100", "0095", "0595", "0090", "1090")),
         countrycode   = as.factor(countrycode),
         g             = round(g, digits = 3)
         ) %>%
  ungroup()



#----------------------------------------------------------
# prepare for Charts
#----------------------------------------------------------


#--------- Find countries with largest changes
n_ctries    <- 20
slted_types <- c("0100", "0095", "0595", "0090", "1090")
slted_types <- c("0100", "0095")

gdf_p <- gt_l %>%
  filter(gini_type  %in% slted_types) %>%
  arrange(year, -drg) %>%
  distinct(year, countrycode) %>%
  group_by(year) %>%
  filter(row_number() <= n_ctries) %>%
  inner_join(gt_l %>%
               filter(gini_type  %in% slted_types),
             by = c("year", "countrycode")) %>%
  ungroup() %>%
  filter(year == 2015)


#--------- setup colors

uniq_cty <- unique(gdf_p$countryname)
uniq_reg <- unique(gdf_p$region)
uniq_ing <- unique(gdf_p$incomegroup)

cty_color <-  setNames(
  rep(palette, ceiling(length(uniq_cty)/length(palette)))[1:length(uniq_cty)],
  uniq_cty
)

reg_color <- gdf_p %>%
  distinct(region, countryname) %>%
  left_join (
    tibble(
      region = uniq_reg,
      color  = rep(palette, ceiling(length(uniq_reg)/length(palette)))[1:length(uniq_reg)]
    ),
    by = "region"
  ) %>%
  select(countryname, color) %>%
  tibble::deframe()


#----------------------------------------------------------
#   plot charts
#----------------------------------------------------------

#------------ Basic
newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = rg,
                Grouping    = countryname,
                LineColor   = "gray")

# largest change
cst_color <- gdf_p %>%
  arrange(-drg) %>%
  distinct(countryname) %>%
  mutate(
    color = if_else(row_number() == 1, palette[[1]], "gray80")
  ) %>%
  deframe()

newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = rg,
                Grouping    = countryname,
                LineColor   = cst_color)

# second largest change
cst_color <- gdf_p %>%
  arrange(-drg) %>%
  distinct(countryname) %>%
  mutate(
    color = c(palette[1:2], rep("gray80", n()-2))
  ) %>%
  deframe()

newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = rg,
                Grouping    = countryname,
                LineColor   = cst_color)


# Other remarkable examples
cst_color <- gdf_p %>%
  arrange(-drg) %>%
  distinct(countryname) %>%
  mutate(
    color = c(palette[1:5], rep("gray80", n()-5))
  ) %>%
  deframe()

newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = rg,
                Grouping    = countryname,
                LineColor   = cst_color)


#--------- highlight specific groups

# Assign colors to countries
ing_color <- gdf_p %>%
  distinct(incomegroup, countryname) %>%
  left_join (
    tibble(
      incomegroup = uniq_ing,
      color  = rep(palette, ceiling(length(uniq_ing)/length(palette)))[1:length(uniq_ing)]
    ),
    by = "incomegroup"
  )


# High Income countries
cst_color <- ing_color %>%
  mutate(
    color = if_else(incomegroup == uniq_ing[1], color, "gray80")
  ) %>%
  select(countryname, color) %>%
  tibble::deframe()

newggslopegraph(dataframe   = gdf_p,
                  Times       = gini_type,
                  Measurement = rg,
                  Grouping    = countryname,
                  LineColor   = cst_color)


newggslopegraph(dataframe   = gdf_p,
                Times       = gini_type,
                Measurement = g,
                Grouping    = countryname,
                LineColor   = cst_color)



# Low income income countries
cst_color <- ing_color %>%
    mutate(
      color = if_else(incomegroup == uniq_ing[3], color, "gray80")
    ) %>%
    select(countryname, color) %>%
    tibble::deframe()

newggslopegraph(dataframe   = gdf_p,
                  Times       = gini_type,
                  Measurement = rg,
                  Grouping    = countryname,
                  LineColor   = cst_color)


# Lower middle income countries
cst_color <- ing_color %>%
    mutate(
      color = if_else(incomegroup == uniq_ing[2], color, "gray80")
    ) %>%
    select(countryname, color) %>%
    tibble::deframe()

newggslopegraph(dataframe   = gdf_p,
                  Times       = gini_type,
                  Measurement = rg,
                  Grouping    = countryname,
                  LineColor   = cst_color)


# Upper middle income countries
cst_color <- ing_color %>%
    mutate(
      color = if_else(incomegroup == uniq_ing[4], color, "gray80")
    ) %>%
    select(countryname, color) %>%
    tibble::deframe()

newggslopegraph(dataframe   = gdf_p,
                  Times       = gini_type,
                  Measurement = rg,
                  Grouping    = countryname,
                  LineColor   = cst_color)



# By income group

uniq_ing <- unique(gdf_p$incomegroup)
ing_color <- gdf_p %>%
  distinct(incomegroup, countryname) %>%
  left_join (tibble(incomegroup = uniq_ing,
                    color  = rep(palette, ceiling(
                      length(uniq_ing) / length(palette)
                    ))[1:length(uniq_ing)]),
             by = "incomegroup") %>%
  select(countryname, color) %>%
  tibble::deframe()

newggslopegraph(
  dataframe   = gdf_p,
  Times       = gini_type,
  Measurement = rg,
  Grouping    = countryname,
  LineColor   = ing_color
)


#################################################################


custom_colors <- pivot_wider(newgdp,
                             id_cols = Country,
                             names_from = Year,
                             values_from = GDP) %>%
  mutate(difference = Year1979 - Year1970) %>%
  mutate(trend = case_when(
    difference >= 2 ~ "green",
    difference <= -1 ~ "red",
    TRUE ~ "gray"
  )
  ) %>%
  select(Country, trend) %>%
  tibble::deframe()


#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

#----------------------------------------------------------
#
#----------------------------------------------------------

