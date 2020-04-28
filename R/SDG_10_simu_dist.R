library(tidyverse)

source("R/utils.R")

file <- "p:/01.PovcalNet/01.Vintage_control/PRY/PRY_2017_EPH/PRY_2017_EPH_V01_M_V01_A_GMD/Data/PRY_2017_EPH_V01_M_V01_A_GMD_GPWG.dta"

nq   <- 10 # No. of quantiles
df <- haven::read_dta(file) %>%
  mutate(
    y =  welfare/cpi2011/icp2011/365
  ) %>%
  select(y, w = weight) %>%
  arrange(y) %>%
  mutate(
    N  = sum(w, na.rm = TRUE),
    cw = cumsum(w),
    q  =  floor(cw/((N+1)/nq)) + 1,
    src = "original"
  ) %>%
  drop_na()

omean <- weighted.mean(x = df$y, w = df$w, na.rm = TRUE)  # Original mean


ndf <- df %>%
  # Perfect Equality
  bind_rows(
    df %>%
    mutate(
      y = rep(omean, times = nrow(df)),
      src = "perfect equality"
    )
  ) %>%

  # B10% has nothing
  bind_rows(
    df %>%
      mutate(
        y = if_else(q == 1, 0, omean),
        y = y*(omean/weighted.mean(x = y, w = w, na.rm = TRUE)),
        src = "B10 has nothing"
      )
  ) %>%

  # T10% has all
  bind_rows(
    df %>%
      mutate(
        y = if_else(q == 10, omean, 0),
        y = y*(omean/weighted.mean(x = y, w = w, na.rm = TRUE)),
        src = "T10 has everything"
      )
  ) %>%

  # without top 10%
  bind_rows(
    df %>%
      filter(q != 10) %>%
      mutate(
        y = y*(omean/weighted.mean(x = y, w = w, na.rm = TRUE)),
        src = "Without T10"
      )
  ) %>%

  # cummulative Y and W
  group_by(src) %>%
  mutate(
    cy = cumsum(y*w)/max(cumsum(y*w), na.rm = TRUE),
    cw = cumsum(w)/max(cumsum(w), na.rm = TRUE),
  )



# ggplot(data = ndf,
#        aes(
#          x = y,
#          fill = src,
#          color = src,
#          weight = w
#        )
#        ) +
#   geom_density(alpha = 0.1) +
#   scale_x_continuous(
#     trans = "log",
#     labels = scales::number_format(accuracy = 0.01,
#                                    decimal.mark = ',')
#   ) +
#   geom_vline(xintercept = omean) +
#   theme_classic()
#
#
#
# ggplot(data=ndf,
#        aes(x=cw,
#            y=cy,
#            color = src)) +
#   geom_line() +
#   scale_x_continuous(name="Cumulative share of population", limits=c(0,1)) +
#   scale_y_continuous(name="Cumulative share of welfare", limits=c(0,1)) +
#   theme_classic()
#
#
#
# ndf %>%
#   summarise(
#     gini = gini(y,w)
#   )
#
# ndf %>%
#   summarise(
#     mean = weighted.mean(y,w, na.rm = TRUE)
#   )
