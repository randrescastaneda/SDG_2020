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

library("data.table")
library("janitor")
library("here")
library("plotly")
library("ggrepel")



#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
metadata_path <- "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/506801/povcalnet_comparability.csv"

md  <- readr::read_csv(metadata_path)
cr  <- readr::read_rds("data/cty_regs_names.rds")
dfc <- readr::read_rds("data/dfc.rds")

DT  <- as.data.table(dfc)
cr  <- as.data.table(cr)
md  <- as.data.table(md)

# load headcount of 50 percent the median

pcnvars <- c("countrycode", "year", "coveragetype", "datatype", "headcount", "povertyline")
srtvars <- c("countrycode", "year", "coveragetype", "datatype")

dm <- readr::read_rds(here("data", "SDG_10_50percent_median.rds"))
dm <- as.data.table(dm)
setkeyv(dm, srtvars)

dm <- dm[,
          ..pcnvars
        ][,
          median := povertyline*2
          ][cr,
              on = "countrycode",
              countryname := i.countryname
          ][,
            text := paste("Country: ",   countrycode, "\n",
                          "year: ",      year, "\n",
                          "Median: ",    median, "\n",
                          "Headcount: ", headcount)
          ]

dm <- unique(dm)


# Fix comparability metadata
cols <- c("coveragetype", "datatype")
md[,
   (cols) := lapply(.SD, as.character),
   .SDcols = cols
   ][,
     coveragetype := fcase(coveragetype == "1", "R",
                           coveragetype == "2", "U",
                           coveragetype == "3", "N",
                           coveragetype == "4", "A"
                           )
     ][,
       datatype := fifelse(datatype == "1", "consumption", "income")
       ]

# Merge with comparability metadata
dm[md,
   on = .(countrycode, coveragetype, datatype, year),
   compare := i.comparability
   ]

# add text variable
dm <- dm[!is.na(compare)
         ][,
           mcomp := compare == max(compare),
           by = .(countryname, datatype, coveragetype)
           ][,
               ggtext := fifelse(year == max(year) | year == min(year),
                                 paste0(countryname, "\n", year), NA_character_
               ),
               by = .(countryname, datatype, coveragetype)
             ]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   poverty rate and median value   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts_to_show <- c("RUS")
cts_to_show <- c("GHA", "CHN", "BRA", "ARG", "GRC", "RUS", "BEL", "SWE")
setorder(dm, countrycode, coveragetype, datatype, year)

dmf <- dm[countrycode %chin% cts_to_show
          & !(countrycode == "CHN" & coveragetype %chin% c("R", "U"))
      ]

setorder(dmf, countrycode, coveragetype, datatype, year)
anyDuplicated(dmf, by=c("countrycode", "year"))

hc_md <- ggplot(dmf, aes(x     = median,
                         y     = headcount,
                         color = countrycode,
                         label = ggtext,
                         group = countrycode)
  ) +
  geom_point() +
  geom_path() +
  geom_text_repel(size = 3,
                  min.segment.length = 0) +
  scale_x_continuous(trans  = "log2",
                     breaks = c(1, 2, 5, 10, 20, 50)
                    ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
hc_md
