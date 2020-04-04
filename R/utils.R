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
                     step = .05,
                     pl = 0.01,
                     maxiter = 5,
                     giveup_lim  = 10) {
  print(paste("workging on", country, year))
  h      <- 0
  pl     <- pl
  r      <- tibble()
  giveup <- 0
  f  <- 0  # no failure

  while (h <= .9999) {
    tryCatch(
      expr = {
        # Your code...
        h0 <- h
        df <- povcalnet(
          country = country,
          povline = pl,
          year = year,
          fill_gaps = TRUE
        )

        h  <- df[["headcount"]]

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
          re <- list(message = e$message,
                     iteration = pl)
          return(re)
        }
      } # end of finally section

    ) # End of trycatch

  } # end of while

  return(r)

}
