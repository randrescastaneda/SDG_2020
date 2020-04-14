library("tidyverse")
library("povcalnetR")

add_and <- function(x) {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], "and", x[2])
  }
  else {
    y <- c(x[1:lx - 1], paste("and", x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}


source("R/povcalnet_iterate.R")
library("povcalnetR")
rcv_dist <- function(country,
                     year,
                     coverage,
                     step = .05,
                     pl = 0.01,
                     maxiter = 5,
                     giveup_lim  = 10) {
  print(paste("workging on", country, year))
  h      <- 0
  pl     <- pl
  r      <- tibble()
  giveup <- 0
  f      <- 0  # no failure

  while (h <= .9999) {
    tryCatch(
      expr = {
        h0 <- h
        df <- povcalnet(
          country = country,
          povline = pl,
          coverage = coverage,
          year = year,
          fill_gaps = TRUE
        )

        h  <- df[["headcount"]]

        # is legnth of h is zero, try five times
        if (length(h) == 0) {
          f <- f + 1
          pl <-  pl + step
          h <- h0
          if (f <= maxiter) {
            next
          } else {
            r <- tibble(message = paste("NO data available in", country, year),
                        iteration = pl)
            break
          }
        }

        # Only save if h > h0. Sometimes, API does not work correctly.
        if (h < h0) {
          f <- f + 1
          pl <-  pl + step
          if (f <= maxiter) {
            next
          } else {
            r <- tibble(message = paste("headcount lower at higher pl", country, year),
                        iteration = pl)
            break
          }
        }

        # if not identical poverty, then save
        if (!(identical(round(h, digits = 4), round(h0, digits = 4)))) {
          r  <- bind_rows(r, df)
        }

        pl <-  pl + step
        f  <- 0  # no failure
      },
      # end of expr section

      error = function(e) {
        f <-  f + 1
        if (f > maxiter) {
          pl <-  pl + step
          giveup <- giveup + 1
        }
        if (giveup > giveup_lim) {
          r <- tibble(message = e$message,
                      iteration = pl)
        }
      }, # end of error section

      warning = function(w) {
        print(paste("warning in", country, year, pl))
        print(w)
      }
    ) # End of trycatch

  } # end of while

  return(r)

}

plain <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  #axis.title = element_blank(),
  plot.title = element_text(hjust = 0.5),
  # legend.position = "bottom",
  legend.position = "none",
  legend.box = "horizontal"
)

palette <- c("#1D6996", "#EDAD08", "#0F8554", "#DCB0F2",
             "#66C5CC", "#F6CF71", "#F89C74", "#5F4690",
             "#E17C05", "#CC503E", "#94346E", "#6F4070",
             "#855C75", "#D9AF6B", "#AF6458", "#736F4C",
             "#68855C", "#9C9C5E", "#A06177", "#8C785D",
             "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7",
             "#38A6A5", "#73AF48", "#87C55F", "#9EB9F3",
             "#526A83", "#625377", "#994E95", "#666666",
             "#467378", "#7C7C7C", "#D3B484", "#B3B3B3"
)
# scales::show_col(palette[1:12])


add_and <- function(x) {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], "and", x[2])
  }
  else {
    y <- c(x[1:lx-1], paste("and", x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}


gini <- function(y, w = NULL) {

  # if (dplyr::is_grouped_df(.data)) {
  #   return(dplyr::do(.data, gini(.)))
  # }
  #
  # y <- deparse(substitute(y))
  # w <- deparse(substitute(w))
  #
  # y    <- .data[[y]]   # welfare
  #
  # if (w == "NULL") {
  #   w = rep(1, times = length(y))
  # } else {
  #   w    <- .data[[w]]    # weigth
  # }

  ordy <- order(y)     # order of y

  w    <- w[ordy]      #order weight
  y    <- y[ordy]      # order welfare

  N    <- sum(w)       # population size
  Y    <- sum(y*w)     # total welfare

  cw   <- cumsum(w)    # Cumulative weights
  cy   <- cumsum(y*w)  # Cumulative welfare

  sn   <-  w/N         # share of population
  my   <- weighted.mean(y, w, na.rm = TRUE)

  i    <- (2*cw - w + 1)/2
  t2   <- y*(N - i + 1)
  gini <- 1+(1/N) - (2/(my*N^2))*sum(t2*w, na.rm = TRUE)
  return(gini)
}

# gini <- function(.data, y, w = NULL) {
#
#   if (dplyr::is_grouped_df(.data)) {
#     return(dplyr::do(.data, gini(.)))
#   }
#
#   y <- deparse(substitute(y))
#   w <- deparse(substitute(w))
#
#   y    <- .data[[y]]   # welfare
#
#   if (w == "NULL") {
#     w = rep(1, times = length(y))
#   } else {
#     w    <- .data[[w]]    # weigth
#   }
#
#   ordy <- order(y)     # order of y
#
#   w    <- w[ordy]      #order weight
#   y    <- y[ordy]      # order welfare
#
#   N    <- sum(w)       # population size
#   Y    <- sum(y*w)     # total welfare
#
#   cw   <- cumsum(w)    # Cumulative weights
#   cy   <- cumsum(y*w)  # Cumulative welfare
#
#   sn   <-  w/N         # share of population
#   my   <- weighted.mean(y, w, na.rm = TRUE)
#
#   i    <- (2*cw - w + 1)/2
#   t2   <- y*(N - i + 1)
#   gini <- 1+(1/N) - (2/(my*N^2))*sum(t2*w, na.rm = TRUE)
#   return(gini)
# }
