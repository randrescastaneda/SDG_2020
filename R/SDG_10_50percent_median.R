# ==================================================
# project:       50 percent of median
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-07-14
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

library("here")
library("pins")
library("tidyverse")
library("plotly")
library("data.table")
library("ggrepel")
# board_register("rsconnect", server = "http://localhost:3939")




#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------

# load headcount of 50 percent the median
board_register("rsconnect", server = "http://w0lxopshyprd1b.worldbank.org:3939/")

# Retrieve Pin
cr <- readr::read_rds(here("data", "cty_regs_names.rds"))
cr <- as.data.table(cr)

dm <- pin_get("acastanedaa/50percent_median_country", board = "rsconnect")
dm <- as.data.table(dm)

first_year <- 2000

dfil <- dm[mcomp == TRUE
         ][
           year >= (first_year)
         ][,
           # Find max and min year available for each country after 2000
           myear := year  %in% c(min(year), max(year)),
           by = .(countrycode, coveragetype)

         ][
           myear == TRUE # keep only those
           ][,
             # find which countries have only one
             # observation and drop them
             nc := .N,
             by = .(countrycode, coveragetype)
             ][
               nc == 2
               ][,
                 c("nc", "myear"):= NULL
                 ][,
                   # seq of year
                   yseq := paste0("yr",as.character(rowid(countrycode, coveragetype)))
                   ]

# Reshape

dw <- dcast(dfil,
            countrycode + countryname + coveragetype ~ yseq,
            value.var = c("headcount", "year"))

# clean
min_diff <- 5
dw <- dw[,
   `:=`(
        difyr = (year_yr2 - year_yr1 >= ..min_diff ),
        difhc = (headcount_yr2 - headcount_yr1)/(year_yr2 - year_yr1),
        highlight = "high"
     )
   ][
     difyr == TRUE
     ][,
       text  := paste("Country:",paste0(countryname, "-", coveragetype), "\n",
                     "Period:", paste0(year_yr2, "-", year_yr1), "\n",
                     "change:", paste0(scales::percent(headcount_yr2, accuracy = .01), "-",
                                       scales::percent(headcount_yr1, accuracy = .01), "=",
                                       scales::percent(difhc, accuracy = .01)), "\n")
       ]

setorder(dw, -difhc)
dw[,
   cty := factor(paste0(countrycode, "-", coveragetype),
                  paste0(countrycode, "-", coveragetype))
   ]

dw <- dw[cr,
   on = .(countrycode)]


#--------- set up for chart ---------

hc_50med <- function(dw, ...) {
  my_colors        <- c("#004B6B", "#47BCEE")
  names(my_colors) <- c("high", "low")

  my_alphas        <-  c(1, .1)
  names(my_alphas) <- c("high", "low")

  my_sizes         <-  c(2, 1)
  names(my_sizes)  <- c("high", "low")


  minx <- NA
  maxx <- NA
  aty  <- 10
  sty  <- 11

  dots <- rlang::enquos(...)
  if (length(dots) != 0) {
    dw <- dw %>%
      mutate(
        highlight = if_else(!!!dots, "high", "low")
      )

  } else {

    dw <- dw %>%
      mutate(
        highlight = "high"
      )
  }



  p <- ggplot(data = dw,
              aes(y = cty,
                  text = text)) +
    geom_segment(
      aes(yend    = cty,
          x       = 0,
          xend    = difhc,
          alpha   = highlight),
      color   = my_colors[1]
    ) +
    geom_point(
      aes(x       = difhc,
          alpha   = highlight,
          size    = highlight),
      fill        = "white",
      color   = my_colors[1],
      shape       = 21,
      stroke      = 0.5
    )  +
    geom_vline(xintercept      = 0,
               color           = "red",
               linetype        = "dashed")  +
    theme_minimal() +
    theme(
      axis.text.y       = element_blank(),
      axis.title.y      = element_blank(),
      # strip.text.y.left = element_text(size = sty),
      legend.position   = "none"
    ) +
    labs(
      title               = "Average Change in Share of population below 50% of median",
      subtitle            = "Sorted by top countries",
      caption             = "Data from PovcalNet",
      x                   = "Change in headcount",
      y                   = ""
    ) +
    scale_x_continuous(limits = c(minx, maxx)) +
    scale_color_manual(values = my_colors) +
    scale_alpha_manual(values = my_alphas) +
    scale_size_manual(values = my_sizes)

  gp <- ggplotly(p, tooltip = "text")
  return(gp)
}

hc_50 <- hc_50med(dw)
# hc_50med(dw, incomegroup == "High income")
#
# hc_50med(dw, incomegroup == "Lower middle income")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   poverty rate and median value   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cts_to_show <- c("RUS")
# cts_to_show <- c("GHA", "CHN", "BRA", "ARG", "GRC", "RUS", "BEL", "SWE")
# setorder(dm, countrycode, coveragetype, datatype, year)
#
# dmf <- dm[countrycode %chin% cts_to_show
#           & !(countrycode == "CHN" & coveragetype %chin% c("R", "U"))
#       ]
#
# setorder(dmf, countrycode, coveragetype, datatype, year)
# anyDuplicated(dmf, by=c("countrycode", "year"))
#
# hc_md <- ggplot(dmf, aes(x     = median,
#                          y     = headcount,
#                          color = countrycode,
#                          label = ggtext,
#                          group = countrycode)
#   ) +
#   geom_point() +
#   geom_path() +
#   geom_text_repel(size = 3,
#                   min.segment.length = 0) +
#   scale_x_continuous(trans  = "log2",
#                      breaks = c(1, 2, 5, 10, 20, 50)
#                     ) +
#   theme_minimal() +
#   theme(
#     legend.position = "none"
#   )
# # hc_md




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Median and headcount   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


miny <- 2000
maxy <- 2017


df <- dm[cr,
         on = .(countrycode)
][,
  `:=`(
    maxc = abs(year - maxy) == min(abs(year - maxy), na.rm = TRUE),
    minc = abs(year - miny) == min(abs(year - miny), na.rm = TRUE)
  ),
  # maxy := year == max(year, na.rm = TRUE),
  by = .(countrycode, coveragetype)
][
  maxc == TRUE | minc == TRUE
][,
  # find which countries have only one
  # observation and drop them
  nc := .N,
  by = .(countrycode, coveragetype)
][
  nc == 2
][,
  # seq of year
  `:=`(
    id = paste0(countrycode, "-", coveragetype),
    yseq = paste0("yr",as.character(rowid(countrycode, coveragetype)))
    )
][,
  c("nc", "maxc", "minc"):= NULL
][
  year == maxy | year == miny
][
  order(year, -headcount),
  ranking := rowid(year)
]


gp_compare <- ggplot(df,
                     aes(x     = headcount,
                         y     = median,
                         color = region,
                         text  = text)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::dollar) +
  facet_grid(~year) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank()
  )
# ggplotly(gp_compare, tooltip = "text")




#--------- 45 degree line ---------
# clean
d45 <- dcast(df,
            region + countrycode + countryname + coveragetype ~ yseq,
            value.var = c("headcount", "year"))

# drop years with no change
d45 <- d45[headcount_yr1 != headcount_yr2]


max1 <-  max(d45$headcount_yr1, na.rm = TRUE)
max2 <-  max(d45$headcount_yr2, na.rm = TRUE)
min1 <-  min(d45$headcount_yr1, na.rm = TRUE)
min2 <-  min(d45$headcount_yr2, na.rm = TRUE)

med45 <- ggplot(d45,
                aes(x = headcount_yr1,
                    y = headcount_yr2,
                    color = region)) +
  geom_point(size = 2,
             show.legend = FALSE) +
  geom_abline(intercept = 0 ,
              slope = 1,
              color = "grey50") +
  labs(x = paste("headcount in circa", miny),
       y = paste("headcount in circa", maxy),
       title = "Share of population below 50% of the median") +
  scale_x_continuous(limits = c(min(min1, min2), max1),
                     labels = scales::percent) +
  scale_y_continuous(limits = c(min(min1, min2), max2),
                     labels = scales::percent) +
  theme_classic() +
  theme(
    legend.position = "",
  )

# ggplotly(med45)


#--------- Rankin ---------

#
# medrank <- ggplot(data = df,
#              aes(x = year,
#                  y = ranking,
#                  group = id)
#              ) +
#   geom_line(aes(color = region),
#             alpha = 1,
#             size = 1
#             ) +
#   geom_point(aes(fill  = countrycode),
#              color = "grey",
#              size = 2) +
#   scale_y_reverse(breaks = 1:nrow(df)) +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank()
#   ) +
#   labs(x        = "Year",
#        y        = "Rank",
#        title    = "Change in ranking over time of percentile",
#        subtitle = "Dots color refer to decile group"
#   ) +
#   ranking_theme()
# medrank
# ggplotly(medrank)
#


df2 <- df[year == maxy
][,
  hc2 := headcount
][, c("countrycode", "coveragetype",  "hc2")
]


df1 <- df[year == miny
][,
  hc1 := headcount
][, c("countrycode", "coveragetype",  "hc1")
]

dfr <- df[df1,
          on = .(countrycode, coveragetype)
][df2,
  on = .(countrycode, coveragetype)
][!is.na(year)
]


medrank <- ggplot(dfr) +
  geom_point(aes(x     = headcount,
                 y     = reorder(id, hc1),
                 color = as.character(year))
  ) +
  theme_minimal() +
  theme(
    axis.text.y     = element_text(size = 6),
    axis.title.y    = element_blank(),
    legend.title    = element_blank(),
    legend.position = c(.8,.2)
  ) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title    = "Growth of Bottom40 and overall mean (Sorted by B40)",
    subtitle = "circa 2012-2017",
    x        = "Annualized growth"
  )


