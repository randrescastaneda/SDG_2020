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
  ][,
    # Create nowcasted series
    nowcasted := fifelse(year  %in% c(2017, 2018, 2019), actual, NA_real_)
  ][,
    # Remove actual estimates from nowcasted years
    actual := fifelse(year  %in% c(2018, 2019), NA_real_, actual)
  ]


setnames(DQ,
         old =  c("baseline", "downside"),
         new =  c("optimistic", "pesimistic")
)

l[["projections"]]  <- DQ



# fan
DD <-
  DC[
    year      >= (2015)
    & povline == "1.9"
    & growth  != "precovid"
  ][
    ,
    `:=`(
      wr = fgt == max(fgt, na.rm = TRUE),
      bs = fgt == min(fgt, na.rm = TRUE)
    ),
    by = .(year)
  ][
    wr == TRUE | bs == TRUE | scenario == "D"
  ]

DD <- dcast(DD,
            year ~ scenario + growth,
            value.var = c("fgt"),
            fun.aggregate = mean
)

DD[,
   c("B_baseline", "G_downside") := NULL
   ][,
     D_actual := fifelse(year == 2019, D_baseline, D_actual)
     ]

setnames(DD,
         old =  c("B_downside", "D_actual", "D_baseline", "D_downside", "G_baseline"),
         new =  c("worst", "actual", "optimistic", "pesimistic", "best")
         )

DD[,
   # Create nowcasted series
   nowcasted := fifelse(year  %in% c(2017, 2018, 2019), actual, NA_real_)
    ][,
      # Remove actual estimates from nowcasted years
      actual := fifelse(year  %in% c(2018, 2019), NA_real_, actual)
    ]

l[["fan_projections"]]  <- DD

#----------------------------------------------------------
#   National data
#----------------------------------------------------------

source(here("R", "SDG_01_NatLines.R"))
wb_codes <- readr::read_csv("Data/wb3_to_region.csv")

nt_df <- overall %>%
  left_join(wb_codes,
            by = c("countrycode" = "iso3c")
            ) %>%
  select(
    countrycode,
    countryname,
    region     = region_iso3c,
    year1      = Iny,
    year2      = Fny,
    pov1       = Value0,
    pov2       = Value,
    abs_gr     = Growth
    ) %>%
  arrange(-abs_gr) %>%
  mutate(
    category = case_when(
      abs_gr  > 0               ~ "Countries with increasing poverty",
      -.5 < abs_gr & abs_gr < 0 ~ "Countries not on track to halve poverty",
      abs_gr < -.5              ~ "Countries on track to halve poverty",
      TRUE ~ ""
    )
  ) %>%
  select(-abs_gr) %>%
  filter( category != "")


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
  filter(!is.na(headcount)) %>%
  mutate(
    world_pop  = population*1e6,
    poor_pop   = headcount*world_pop,
    `else`     = world_pop - poor_pop
  ) %>%
  select(year, world_pop, poor_pop, `else`) %>%
  arrange(year)

wyears <- unique(warea$year)

carea <- cty %>%
  filter(
    countrycode  %in% c("CHN", "IND", "NGA"),
    year         %in% wyears
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
    regioncode == "SSA",
    year      %in% wyears
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

readr::write_rds(l, "data/SDG01_updated_data.rds")

writexl::write_xlsx(
  x         = l,
  path      = "data/updated_data.xlsx",
  col_names = TRUE
)
