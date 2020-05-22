setwd("C:/Users/wb562350/OneDrive - WBG/Documents/Git/Research/SDG_2020")

library(wbstats)
library(tidyverse)
library(data.table)
library(scales)
library(hrbrthemes)
library(plotly)

# functions
source("R/panel_WDI.R") # Wrapper to wb function from wbstat

# parameters
ordervariable <-
  "incomegroup" # how to group countries, works with region, incomegroup or both c("region","incomegroup")
sortvariable <-
  "Growth40" # How to sort values within groups Growth40, Growth, diff, diffabs?
rankvariable <-
  c("diff", "Growth40") # How to rank countries: Growth40, Growth, diff, diffabs?
filename <- "" # root file name


# ## Keep countries in older points
# list2017 <- c("CHN", "BEL", "ECU", "KAZ", "BTN", "KHM", "BOL", "BRA", "RUS",
#               "COL", "PER", "CHL", "URY", "MKD", "THA", "MDA", "GEO", "VNM",
#               "PAN", "UKR", "IDN", "SLV", "UGA", "TZA", "NOR", "IRN", "PAK",
#               "TGO", "ROU", "POL", "CHE", "LKA", "PHL", "FIN", "ARG", "DOM",
#               "DEU", "BEL", "KGZ", "RWA", "NLD", "USA", "ALB", "GRB", "PRT",
#               "HUN", "LVA", "ISL")


# ------------------ code ---------------------#


# Load Share Prosperity Data
WDI <-
  read.csv2(
    "data/WDI2020.csv",
    sep = ",",
    dec = ".",
    stringsAsFactors = F
  )

WDI <- WDI %>%
  rename(Growth40 = bottom_growth) %>%
  rename(Growth = total_growth) %>%
  rename(GrowthMedian = median_growth) %>%
  rename(countrycode = code) %>%
  mutate(
    region = ifelse(grepl("High Income", region), "OHI", as.character(region)),
    diff = (Growth40 - Growth),
    diffabs = abs(diff)
  )

# Load the old WDI data
WDI2017 <-
  read.csv2(
    "data/WDI2017.csv",
    sep = ",",
    dec = ".",
    stringsAsFactors = F
  )

WDI2017 <- WDI2017 %>%
  rename(
    countryname = Country.Name,
    countrycode = Country.Code,
    indicatorID = Indicator.Code,
    indicator = Indicator.Name,
  ) %>%
  gather("year", "value", X1960:X2016) %>%
  extract(year, into = "Year", regex = "([0-9]+)") %>%
  mutate(Year = as.numeric(as.character(Year)))

# I'll keep just those indicatores I care for
WDI2017 <- WDI2017 %>%
  filter(indicatorID %in% c("SI.SPR.PC40.ZG", "SI.SPR.PCAP.ZG")) %>%
  drop_na() %>%
  select(-indicator, -countryname) %>%
  spread(indicatorID, "value") %>%
  rename(
    OLDGrowth40 = SI.SPR.PC40.ZG,
    OLDGrowth = SI.SPR.PCAP.ZG,
    OLDyear = Year
  ) %>%
  mutate(
    OLDdiff = (OLDGrowth40 - OLDGrowth),
    OLDdiffabs = abs(OLDdiff)
  )


# join data

WDI <- full_join(WDI, WDI2017, by = "countrycode") %>%
  select(-region)


# -- Add Region ID
cr <- read_rds("data/cty_regs_names.rds") %>%
  setDT() %>%
  select(-lending)

WDI <- left_join(WDI, cr, by = "countrycode") %>%
  select(-country) %>%
  rename(country = countryname) %>%
  mutate(
    country = if_else(countrycode == "KHM", "Cambodia", country),
    region = if_else(countrycode == "KHM", "EAP", region),
    incomegroup = if_else(countrycode == "KHM", "Lower middle income", incomegroup),
    regionname = if_else(countrycode == "KHM", "East Asia and Pacific", regionname)
  )

# =============#
#   plots     #
# =============#

### 2017 points ###

# data <- WDI %>%
# filter(countrycode %in% list2017)

temp         <- sortvariable
sortvariable <- paste0("OLD", sortvariable)

data <- WDI %>%
  filter(OLDdiff > 0 & !is.na(diff & OLDdiff))

# data <- data %>%
#   rowwise() %>%
#   mutate_(ordervar = ordervariable) %>%
#   mutate_(sortvar = sortvariable) %>%
#   arrange(ordervar, sortvar)



data <-  data %>%
  mutate(ordervar = !! sym(ordervariable)) %>%
  mutate(sortvar  =  !! sym(temp)) %>%
  arrange(ordervar, sortvar)


# data$id <- seq(1, nrow(data))

# empy space between regions
empty_bar        <- 1
to_add           <- data.frame(matrix(NA, empty_bar * length(unique(data$ordervar)), ncol(data)))
colnames(to_add) <- colnames(data)
to_add$ordervar  <- rep(unique(data$ordervar), each = empty_bar)
data             <- rbind(data, to_add)
data             <- data %>% arrange(ordervar, sortvar)
data$id          <- seq(1, nrow(data))

# set lines
a <- data %>%
  group_by(ordervar) %>%
  summarise(max = max(id), mean = mean(id)) %>%
  ungroup() %>%
  mutate(val = max(max) + 2)

data <- data %>%
  mutate(
    Growth40 = as.numeric(as.character(Growth40)),
    Growth = as.numeric(as.character(Growth))
  )

breaks <- seq(min(data$Growth40, na.rm = T),
  max(data$Growth40, na.rm = T),
  length.out = 5
)


# Plot
p <- ggplot(data) +
  geom_segment(aes(
    y = id,
    yend = id,
    x = Growth40,
    xend = Growth
  ),
  color = rgb(0, 0, 0, 0.3)
  ) +
  geom_point(
    aes(y = id, x = Growth, text = country),
    color = "black",
    fill = "white",
    shape = 21,
    stroke = 0.7,
    size = 2
  ) +
  geom_point(
    aes(y = id, x = Growth40, text = country),
    color = "white",
    fill = "red",
    shape = 21,
    size = 2
  ) +
  geom_segment(
    aes(
      y = id,
      yend = id,
      x = OLDGrowth40,
      xend = OLDGrowth
    ),
    color = "steelblue4",
    alpha = 0.5
  ) +
  geom_point(
    aes(y = id, x = OLDGrowth, text = country),
    color = "steelblue4",
    fill = "white",
    shape = 21,
    stroke = 0.7,
    size = 2,
    alpha = 0.5
  ) +
  geom_point(
    aes(y = id, x = OLDGrowth40, text = country),
    color = "white",
    fill = "steelblue4",
    shape = 21,
    size = 2,
    alpha = 0.5
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
  ) +
  xlab("Growth") +
  ylab("") +
  geom_text(
    data = data,
    aes(y = id, x = -7, label = country),
    angle = 0,
    alpha = 0.6,
    size = 2
  ) +
  geom_text(
    data = a,
    aes(y = mean, x = 8, label = ordervar),
    angle = 0,
    alpha = 0.6,
    size = 3
  ) +
  geom_segment(
    data = a,
    aes(
      y = max,
      yend = max,
      x = -5,
      xend = 10
    ),
    color = rgb(0, 0, 0, 0.3)
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  ggtitle("Growth Bottom 40 vs National Average - 2012-2017", subtitle = "Old top countries") +
  labs(
    caption = paste(
      "Bottom and Total growth as reported in the Global Database of Share Prosperity."
    )
  )

p

p2s <- p
p2 <- ggplotly(p)

sortvariable <- temp
