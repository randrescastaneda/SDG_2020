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


source("R/povcalnet_iterate.R")
library("povcalnetR")
rcv_dist <- function(country, year, qtl = 500) {

  print(paste("workging on", country, year))
  tryCatch(
    expr = {
      # Your code...
      max_th <- povcalnet_iterate(country = country,
                                  year = year,
                                  goal = .999,
                                  tolerance = 4) %>%
        select(threshold) %>%
        pull() %>%
        ceiling()

      step <- round(max_th/qtl, digits = 2)

      pls <- seq(from = 0.01,
                 to = max_th,
                 by = step)

      if (pls[length(pls)] < max_th) {
        pls <- c(pls, max_th)
      }

      pb <- progress_bar$new(total = length(pls))
      povcalcall <- function(pl, country, year) {
        pb$tick()
        df <- povcalnet(country = country, povline = pl, year = year)
        return(df)
      }

      cty_data <- map_df(pls, povcalcall, country = country, year = year)


      return(cty_data)
    }, # end of expr section

    error = function(e) {
      return(e$message)
    } # end of error section

  ) # End of trycatch

}

