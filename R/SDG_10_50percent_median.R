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

dm <- dm[mcomp == TRUE
         ][
           year >= (first_year)
         ][,
           # Find max and min year available for each country after 200
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

dw <- dcast(dm,
            countrycode + countryname + coveragetype ~ yseq,
            value.var = c("headcount", "year"))

# clean
min_diff <- 5
dw <- dw[,
   `:=`(
        difyr = (year_yr2 - year_yr1 >= ..min_diff ),
        difhc = headcount_yr2 - headcount_yr1,
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

setorder(dw, difhc)
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
      title               = "Change in Share of population below 50% of median",
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
# hc_md
