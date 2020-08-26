#----------------------------------------------------------
#   Data for Elbert
#----------------------------------------------------------

library(here)
source(here("R", "SDG_01_data.R"))
source(here("R", "SDG_01_NatLines.R"))


#--------- Country data


cty_df <- cty %>%
  mutate(
    poor_pop = round(headcount * population, 2)
  ) %>%
  select(-c(regionf, text))

write_csv(x         = cty_df,
          path      = "data/SDG01_Country_data.csv",
          col_names = TRUE,
          na        ="")

#--------- World data


wld_df <- pr_wld %>%
  mutate(case =
           case_when(
             (alpha == -2 & extragrowth == 2)    ~ "Best",
             (alpha == 2  & extragrowth == -2)   ~ "Worst",
             (alpha == 0  & extragrowth == 0)    ~ "Most likely",
             (is.na(alpha) & is.na(extragrowth)) ~ "Actual",
             TRUE ~ ""
           )) %>%
  filter(case != "") %>%
  select(-c(alpha, extragrowth)) %>%
  left_join(select(wld, poor_pop, year))

write_csv(x         = wld_df,
          path      = "data/SDG01_global_data.csv",
          col_names = TRUE,
          na        = "")


# Global poverty trend and goal
yv <- tibble( year = c(2016:2030))

wld2 <- wld %>%
  select(year, headcount) %>%
  arrange(year) %>%
  bind_rows(yv)

# liner model
lm_fit <-  lm(headcount ~ year,
              data = wld2)

lm_wld <- data.frame(wld2,
                     hc_proj = predict(lm_fit, wld2))


write_csv(x         = lm_wld,
          path      ="data/lm_projection_global.csv",
          col_names = TRUE,
          na        ="")


# projectios by country

cty_rp <- cty %>%
  select(countrycode, year, headcount, region) %>%
  bind_rows(cty_p) %>%
  select(-beta) %>%
  arrange(countrycode, year)


write_csv(x         = cty_rp,
          path      ="data/SDG01_Country_graduate_year.csv",
          col_names = TRUE,
          na        ="")


# p <- ggplot(data = filter(cty, countrycode == "COL"),
#             aes(x = year,
#                 y = headcount)) +
#   geom_point() +
#   stat_smooth(method = "lm",
#               fullrange = TRUE,
#               se = FALSE,
#               color = "grey50")
# ggplotly(p)


#--- National data

nt_df <- overall %>%
  select(
    countrycode,
    countryname,
    projection = project,
    year1      = Iny,
    year2      = Fny,
    pov1       = Value0,
    pov2       = Value,
    abs_gr     = Growth,
    ann_gr     = GAGR,
    abs_gr_p   = Growthp
    )


write_csv(x         = nt_df,
          path      ="data/SDG01_national_lines.csv",
          col_names = TRUE,
          na        ="")


#---- Country projections

bad_ctrs2 <- bad_ctrs %>%
  select(-c(ymm, text, regionf, poor_pop))

write_csv(x         = bad_ctrs2,
          path      ="data/SDG01_bad_countries.csv",
          col_names = TRUE,
          na        ="")

