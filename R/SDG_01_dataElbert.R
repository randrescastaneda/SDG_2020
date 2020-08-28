#----------------------------------------------------------
#   Data for Elbert
#----------------------------------------------------------

library(here)
library(data.table)

# Initialize data
l <- vector("list")

#----------------------------------------------------------
#   Country data
#----------------------------------------------------------

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
l[["countries"]] <- cty_df

write_csv(x         = cty_df,
          path      = "data/SDG01_Country_data.csv",
          col_names = TRUE,
          na        ="")

#----------------------------------------------------------
#   graduation year by country
#----------------------------------------------------------

cty_rp <- cty %>%
  select(countrycode, year, headcount) %>%
  left_join(cr,
            by = "countrycode") %>%
  bind_rows(cty_p) %>%
  select(countrycode,
         year,
         headcount,
         region,
         country = countryname) %>%
  arrange(countrycode, year)

l[["graduation"]] <- cty_rp

write_csv(x         = cty_rp,
          path      ="data/SDG01_Country_graduate_year.csv",
          col_names = TRUE,
          na        ="")

#----------------------------------------------------------
#   World data and projections
#----------------------------------------------------------

# Standard Projections
source(here("R","SDG_01_covid_projections.R"))
DQ <-
  DB[
    povline == "1.9"
  ][,
    c("year", "growth", "fgt_D")
  ]

DQ <- dcast(DQ,
            year ~ growth,
            value.var = "fgt_D"
)

DQ[,
   actual := fifelse(year == 2019, baseline, actual)
]

l[["projections"]]  <- DQ


# fan
DD <-
  DC[
    year      >= (2015)
    & povline == "1.9"
  ][
    ,
    `:=`(
      wr = fgt == max(fgt, na.rm = TRUE),
      bs = fgt == min(fgt, na.rm = TRUE)
    ),
    by = .(year)
  ][
    wr == TRUE | bs == TRUE
  ]

DD <- dcast(DD,
            year ~ scenario,
            value.var = c("fgt"),
            fun.aggregate = mean
)

setnames(DD,
         old =  c("B", "D", "G"),
         new =  c("worst", "actual", "best")
         )

l[["fan_projections"]]  <- DD

#----------------------------------------------------------
#   National data
#----------------------------------------------------------

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
    ) %>%
  arrange(-abs_gr) %>%
  mutate(
    category = case_when(
      abs_gr  > 0               ~ "Countries with increasing poverty",
      -.5 < abs_gr & abs_gr < 0 ~ "Countries not on track to halve poverty",
      abs_gr < -.5              ~ "Countries on track to halve poverty",
      TRUE ~ ""
    )
  )

l[["national"]] <- nt_df

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

#----------------------------------------------------------
#   Area Spread sheet
#----------------------------------------------------------

warea <- wld %>%
  mutate(
    world_pop  = population*1e6,
    poor_pop   = headcount*world_pop,
    `else`     = world_pop - poor_pop
  ) %>%
  select(year, world_pop, poor_pop, `else`) %>%
  arrange(year)

carea <- cty %>%
  filter(
    countrycode  %in% c("CHN", "IND", "NGA")
  ) %>%
  mutate(
    poor = population*1e6*headcount
  ) %>%
  select(countryname, year, poor) %>%
  pivot_wider(
    id_cols     = year,
    names_from  = countryname,
    values_from = poor
  )

ssa <- povcalnetR::povcalnet_wb(server = "int") %>%
  filter(
    regioncode == "SSA"
  ) %>%
  mutate(
    poor = population*1e6*headcount
  ) %>%
  select(regiontitle, year, poor) %>%
  pivot_wider(
    id_cols     = year,
    names_from  = regiontitle,
    values_from = poor
  )

area <- left_join(
  warea, carea, by = "year") %>%
  left_join(
    ssa, by = "year"
  ) %>%
  arrange(year)


l[["area"]]  <- area

#----------------------------------------------------------
#   Global poverty trend and goal
#----------------------------------------------------------

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


#----------------------------------------------------------
#   Export to Excel
#----------------------------------------------------------

l[sapply(l, is.null)] <- NULL

writexl::write_xlsx(
  x         = l,
  path      = "data/updated_data.xlsx",
  col_names = TRUE
)
