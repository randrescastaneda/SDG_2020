panel_WDI <- function(indicator, start=2000, end = 2015, maxdist=5, cb = TRUE, long = F){
  
  require(wbstats)
  require(tidyverse)
  require(data.table)
  
  # ==================
  # Check conditions 
  # ==================
  if (is.character(indicator) == F){
    stop("The indicator must be a string")
  }
  if (length(indicator)!= 1){
    print(paste("Constructing panel for",length(indicator),"indicators"))
  }
  if (is.numeric(start)== F){
    stop("The circa year must be numeric")
  }
  if (is.numeric(end)== F){
    stop("The circa year must be numeric")
  }
  if (is.numeric(maxdist)== F){
    stop("The max distance to the circa year must be numeric")
  }
  
  # ===============
  # Query Data
  # ==============
  
  # Download WBG data
  WDI <- wb(indicator = indicator)

  # =============
  # Clean data
  # =============
  
  # var renaming
  WDI <- rename(WDI, countrycode=iso3c) %>% 
    rename(Year=date) %>%
    mutate(Year = as.numeric(Year))
  
  # Add Region ID
  cr <- read_rds("data/cty_regs_names.rds") %>%
    setDT()
  
  cols  <- c("regioncode", "countryname")
  icols <- paste0("i.", c("region", "countryname"))
  
  WDI <- merge(WDI, cr, by.x = "countrycode")
  
  # A codebook
  codebook <- WDI %>% 
    select(indicator, indicatorID) %>% 
    unique()
  
  
  # --- filtering on circa 
  
  WDI <- WDI %>%
    drop_na() %>% 
    group_by(countrycode, indicatorID) %>%
    mutate( usedsy =  if_else(abs(Year - start) == min(abs(Year - start)),Year,0),
            usedsy = max(usedsy),
            usedey =  if_else(abs(Year - end) == min(abs(Year - end)),Year,0),
            usedey = max(usedey)) %>%
    filter(usedsy == Year| usedey == Year) %>%
    filter(abs(usedsy - start) < maxdist,
           abs(usedey - end) < maxdist,            # Only keep those with at most the max distance to the circa
           (n() == 2 | usedsy > start | usedey > start)) %>%        # solve duplicities in favor of the newer point 
    arrange(countrycode) %>%
    ungroup() %>% 
    mutate(Year = if_else(usedsy == Year, start, end))
  
  # ---- Reshape data
  
  if(long == F){
    WDI <- WDI %>% select(-indicator, -usedsy , -usedey)
    WDI <- WDI %>% spread(indicatorID, value)
  }
  
  
  #=============
  # Return data 
  #============
  
  WDI <- as.data.frame(WDI)
  assign("WDI", WDI, envir=.GlobalEnv)
  
  if(cb == TRUE){
    codebook <- as.data.frame(codebook)
    assign("codebook", codebook, envir=.GlobalEnv)
  }
}