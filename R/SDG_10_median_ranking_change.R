# ==================================================
# project:       Change in ranking of medians
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ----------------------------------------------------
# Creation Date:    2020-07-23
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
library("plotly")
library("ggplot2")

#----------------------------------------------------------
#   subfunctions
#----------------------------------------------------------
source("R/utils.R")

qtile <- function(x, nq = 10) {
  N   <-  length(x)
  csw <-  1:N
  qp  <-   floor(csw/((N+1)/nq)) + 1
  return(qp)
}

ranking_theme <- function() {

  # Colors
  color.background = "white"
  color.text       = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background   = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background    = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border       = element_rect(color=color.background)) +
    theme(strip.background   = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks         = element_blank()) +

    # Format the legend
    theme(legend.position    = "none") +

    # Format title and axis labels
    theme(plot.title         = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x       = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y       = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x        = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y        = element_text(size=10, color = color.text)) +
    theme(strip.text         = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin        = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



palette <- c("#5F4690", "#1D6996", "#38A6A5", "#0F8554", "#73AF48", "#EDAD08",
             "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
             "#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2", "#87C55F", "#9EB9F3",
             "#E17C05", "#CC503E", "#94346E", "#6F4070", "#994E95", "#666666",
             "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C",
             "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3")





#----------------------------------------------------------
#   Aux data
#----------------------------------------------------------
cr <- read_rds("data/cty_regs_names.rds")
dfc <- read_rds("data/dfc.rds")


# set data.table
input <- list(
  pc  = 50,
  hc  = 10,
  yrs = c(1993, 2002, 2010, 2015, 2017),
  ctr = c("Norway", "Finland")
)


#----------------------------------------------------------
#   Calculate Quantiles
#----------------------------------------------------------

# set data.table

DT <- as.data.table(dfc)
cr <- as.data.table(cr)
setnames(DT, "threshold", "pv")

# Sort
setorder(DT, year, goal, pv)


DT <- DT[
  # remove old years
  year >= 1990
][
  # filter negative values (which we should not have)
  pv > 0 & !is.na(pv)
][,
  # multiply by 100
  goal := 100*goal

][
  ,
  headcount := NULL
][
    # Merge country names and regions
    cr,
    on = .(countrycode)
  ]


#----------------------------------------------------------
#   Prepare database
#----------------------------------------------------------

# Sort


DQ <-
  DT[
    year  %in% input$yrs
    & goal == input$pc
  ]

setorder(DQ, year, pv)
DQ <-
  DQ[
    ,# Create deciles in each percentile
    qp := qtile(pv),
    by = .(year)
  ][
  order(year, -pv),
  ranking := rowid(year)
]


#--------- Higher and lower countries ---------

CC <- DQ[
  qp  %in% input$hc
  & year  %in% input$yrs
  ][,
    unique(countrycode)
    ]

DQ <- DQ[
  countrycode  %chin%  CC
  ][
    ,
    text := paste0("Country: ", countryname, "-", coverage, "\n",
                   "Year: ", year, "\n",
                   "Rank: ", ranking, "\n",
                   "Decile: ", qp, "\n"
                   )
  ][
    ,
    countrycov := paste0(countrycode, "-", coverage)
  ]

if (!(is.null(input$ctr))) {

  DQ <-
    DQ[
      ,
      flag := ifelse(countryname %in% input$ctr, TRUE, FALSE)
    ][
      ,
      country_col := if_else(flag == TRUE, countrycov, "zzz")
    ]
}




#----------------------------------------------------------
#   First chart
#----------------------------------------------------------

if (!(is.null(input$ctr))) {
  selctr <- deparse(quote(country_col))
} else {
  selctr <- deparse(quote(countrycov))
}

pr <- ggplot(data = DQ,
             aes(x = year,
                 y = ranking,
                 group = countrycov)
             ) +
  geom_line(aes(color = eval(parse(text = selctr)),
                alpha = 1),
            size = 1) +
  geom_point(aes(fill  = as.character(qp),
                 text = text
                 ),
             color = "grey",
             size = 2) +
  scale_y_reverse(breaks = 1:nrow(DQ)) +
  theme(
    legend.position = "none"
  ) +
  labs(x        = "Year",
       y        = "Rank",
       title    = "Change in ranking over time of percentile",
       subtitle = "Dots color refer to decile group"
       )+
  ranking_theme()

if (!(is.null(input$ctr))) {
  lctr <- length(input$ctr)

  pr <- pr +
    scale_color_manual(values = c(palette[1:length(input$ctr)], "grey"))
}


ggplotly(pr, tooltip =  "text")





ctr <- as.character(input$ctr)


if (any(ctr != "")) {
  print(3)
} else {
  selctr <- deparse(quote(countrycov))
}
