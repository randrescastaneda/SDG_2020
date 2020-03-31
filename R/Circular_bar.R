
# library
library(plotly)
library(ggplot2)
library(ggforce)
library(hrbrthemes)
library(viridis)

library(tidyverse)

# some data play
data <- as.data.frame(overall)

data = data %>% arrange(regioncode, Growth)

data = data %>% mutate(Growthp = ifelse(Growthp > 1, 1, Growthp )) %>%
  mutate(Growthp = ifelse(Growthp < -1, -1, Growthp ) )

# define label variable
# data <- data %>%
#   mutate(plotlabel = paste(Term, countrycode, sep = " "))

data$plotlabel <- data$countrycode


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$regioncode), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$regioncode <- rep(levels(data$regioncode), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(regioncode)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>%
  group_by(countrycode) %>%
  summarize(start=min(id), end=max(id) + 1) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# Make the plot
p <-
  ggplot(data, aes(x = as.factor(id), y = Growthp, fill = regioncode)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat = "identity", alpha = 0.3) +
  ylim(-1, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    # panel.grid = element_blank(),
    panel.grid.major = element_line(colour = "gray", size = 0.05),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar("x") +
  geom_text(
    data = label_data,
    aes(
      x = id,
      y = 1,
      label = plotlabel,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 4,
    angle = label_data$angle,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = base_data,
    aes(
      x = start,
      y = -0.5,
      xend = end,
      yend = -0.5
    ),
    colour = "red",
    alpha = 0.8,
    size = 0.6 ,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = base_data,
    aes(
      x = start,
      y = 0,
      xend = end,
      yend = 0
    ),
    colour = "gray",
    alpha = 0.8,
    size = 0.2 ,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = base_data,
    aes(
      x = start,
      y = 0.5,
      xend = end,
      yend = 0.5
    ),
    colour = "gray",
    alpha = 0.8,
    size = 0.2 ,
    inherit.aes = FALSE
  )


plot <- p +
  geom_bar(aes(x = as.factor(id), y = Growth, fill = regioncode),
           stat = "identity",
           alpha = 0.5) +
  theme(legend.position = "right") + labs(fill = "Region")

plot

# ggsave(filename = "NatHC.png",
#        plot = plot,
#        device='png',
#        height = 250, width = 300, dpi = 300,
#        limitsize = F, units = "mm")
