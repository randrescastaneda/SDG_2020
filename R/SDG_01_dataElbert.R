#----------------------------------------------------------
#   Data for Elbert
#----------------------------------------------------------

library(here)
library(data.table)
library("writexl")

# Initialize data
l <- vector("list", 7)
#--------- Country data


source(here("R", "SDG_01_data.R"))
cty_df <- cty %>%
  mutate(
    poor_pop = round(headcount * population, 2)
  ) %>%
  select(code    = countrycode,
         country = countryname,
         year,
         rate    = headcount,
         poor_pop,
         pop     = population,
         region)

# Add to list
l[[1]]       <- cty_df
names(l)[1]  <- "countries"

write_csv(x         = cty_df,
          path      = "data/SDG01_Country_data.csv",
          col_names = TRUE,
          na        ="")

#--------- World data
source(here("R","SDG_01_covid_projections.R"))
DQ <-
  DB[
    povline == "1.9"
    ][,
      c("year", "growth", "fgt_D")
      ]

DQ <- dcast(DQ,
            year ~ growth,
            value.var = fgt_D
            )

DQ[,
   actual := fifelse(year == 2019, baseline, actual)
   ]

l[[2]]       <- DQ
names(l)[2]  <- "projections"


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
source(here("R", "SDG_01_NatLines.R"))
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

