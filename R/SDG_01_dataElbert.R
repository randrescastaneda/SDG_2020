#----------------------------------------------------------
#   Data for Elbert
#----------------------------------------------------------

#--------- Country data


cty_df <- cty %>%
  mutate(
    poor_pop = round(headcount * population, 2)
  ) %>%
  select(-c(regionf, text))

write.csv(cty_df,
          file="data/SDG01_Country_data.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")

#--------- World data


wld_df <- pr_wld %>%
  mutate(case =
           case_when(
             (alpha == -2 & extragrowth == 2)    ~ "Best",
             (alpha == 2 & extragrowth == -2)    ~ "Worst",
             (alpha == 0 & extragrowth == 0)     ~ "Most likely",
             (is.na(alpha) & is.na(extragrowth)) ~ "Actual",
             TRUE ~ ""
           )) %>%
  filter(case != "") %>%
  select(-c(alpha, extragrowth)) %>%
  left_join(select(wld, poor_pop, year))

write.csv(wld_df,
          file="data/SDG01_global_data.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")



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


write.csv(lm_wld,
          file="data/lm_projection_global.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")


# projectios by country

cty_rp <- cty %>%
  select(countrycode, year, headcount, region) %>%
  bind_rows(cty_p) %>%
  select(-beta) %>%
  arrange(countrycode, year)


write.csv(cty_rp,
          file="data/SDG01_Country_graduate_year.csv",
          row.names = FALSE,
          col.names = TRUE,
          na="")


# p <- ggplot(data = filter(cty, countrycode == "ARM"),
#             aes(x = year,
#                 y = headcount)) +
#   geom_point() +
#   stat_smooth(method = "lm",
#               fullrange = TRUE,
#               se = FALSE,
#               color = "grey50")
#
# ggplotly(p)
