get_WDI <- function(indicator, circayr=2015, maxdist=5, cb = TRUE){
 
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
    stop("Only one indicator is allowed")
  }
  if (is.numeric(circayr)== F){
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
  
  cols  <- c("regioncode", "countryname", "incomegroup")
  icols <- paste0("i.", c("region", "countryname", "incomegroup"))
  
  WDI <- merge(WDI, cr, by.x = "countrycode")
  
  # A codebook
  codebook <- WDI %>% 
    select(indicator, indicatorID) %>% 
    unique()
  
  # ---- Reshape data
  
  WDI <- WDI %>% select(-indicator)
  WDI <- WDI %>% spread(indicatorID, value)
  
  # --- filtering on circa 
    
  WDI <- WDI %>%
    drop_na() %>% 
    group_by(countrycode) %>%
    mutate( usedy =  if_else(abs(Year - circayr) == min(abs(Year - circayr)),Year,0),
            usedy = max(usedy)
    ) %>%
    filter(usedy == Year) %>%
    filter(abs(Year - circayr) < maxdist,   # Only keep those with at most the max distance to the circa
    (n() == 1 | Year > circayr)) %>%        # solve duplicities in favor of the newer point 
    arrange(countrycode) %>%
    ungroup() %>% 
    mutate(Year = circayr)
  
  
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