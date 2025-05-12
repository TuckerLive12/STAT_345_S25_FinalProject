library(tigris)
library(tidyverse)
library(sf)

################################################
### TO DO ###
################################################
# Improve get_state_roads by removing the TryCatch block and using the counties()
#  command to extract the COUNTYFP number. 
# Create function that only needs the state ID and return a tibble of all roads in that state
################################################
################################################


# Highest county FP is 840

##
# Takes in the year and state and outputs a tibble of the lowest county FP
#  in state.
#
#
##
get_first_county_roads <- function(state, year) {
  
  counties <- counties(state = state, year = year) |>
    arrange(COUNTYFP)
  
  tibble <- counties$COUNTYFP[1]
  
  return(tibble)
}


##
# Takes in the year and state and outputs a tibble that has the county ID, year, ...
#
#
##
get_state_roads <- function(tibble, state, county, year) {
  
  # If tibble is NULL, call function get_first_county_roads to get the needed tibble.
  if (is.null(tibble)) {
    tibble <- get_first_county_roads(state,year)
  }
  
  # (Grok) Initialize dat to NULL to avoid undefined variable issues
  dat <- NULL
  
  # Try to fetch road data
  tryCatch(
    {
      dat <- roads(state = state, county = county, year = year)
    },
    error = function(e) {
      # Return input tibble unchanged on error
      return(tibble)
    },
    warning = function(w) {
      # Return input tibble unchanged on warning
      return(tibble)
    }
  )
  
  # (Grok) Only proceed if dat is not NULL (i.e., no error/warning occurred) 
  if (!is.null(dat)) {
    dat <- dat |>
      st_drop_geometry() |>
      mutate(
        COUNTY_ID = as.character(county),
        STATE_ID = as.character(state),
        YEAR = as.character(year)  # Ensure consistent type
      ) 
    
    
    # Combine tibbles
    return(bind_rows(tibble, dat))
  }
  
  # (Grok) Fallback: return input tibble if dat is NULL
  return(tibble)
}


state <- 1
tibble <- roads(state, 1,2023)
tibble <- tibble |>
  st_drop_geometry() |>
  mutate(
    COUNTY_ID = as.character(1),
    STATE_ID = as.character(1),
    YEAR = as.character(2023)  
  ) 

for(i in 3:103) {
  tibble <- get_state_roads(tibble, state, i, 2023)
}

glimpse(tibble)

write_csv(tibble, file = "./STAT 345/STAT_345_S25_FinalProject/Data_Scraping_and_Database/Alabama_Roads_2023.csv")

Alabama_Roads_2023 <- tibble

counties <- counties(state = 1) |>
  arrange(COUNTYFP) |>
  print(n=200)

tibble |>
  filter(LINEARID == "110685802980")



# Some checking commands
identical(unique(tibble$LINEARID), tibble$LINEARID)
format(object.size(tibble), units = "auto")

tibble |>
  count(STATE_ID, YEAR, COUNTY_ID)
glimpse(tibble)

