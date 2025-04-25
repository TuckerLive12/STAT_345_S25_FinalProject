library(tigris)
library(tidyverse)



state <- 1
county <- 003
dat <- roads(state = state, county = county, year = 2023)
glimpse(dat)
dat <- dat |>
  mutate(COUNTY_ID = as.character(county), STATE_ID = as.character(state))


roads(12,001)


# Highest county FP is 840

##
# Takes in the year and state and outputs a tibble that has the county ID, year, ...
#
#
##
get_state_roads <- function(tibble, state, county, year) {
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
      mutate(
        COUNTY_ID = as.character(county),
        STATE_ID = as.character(state),
        YEAR = as.character(year)  # Ensure consistent type
      ) |>
      select(-geometry)
    
    # Combine tibbles
    return(bind_rows(tibble, dat))
  }
  
  # (Grok) Fallback: return input tibble if dat is NULL
  return(tibble)
}



state <- 1
tibble <- roads(state, 1,2023)
tibble <- tibble |>
  select(-geometry)

for(i in 3:5) {
  tibble <- get_state_roads(tibble, state, i, 2023)
}

identical(unique(tibble$LINEARID), tibble$LINEARID)
object.size(tibble)

format(object.size(tibble), units = "auto")
format(object.size(tibble$geometry), units = "auto")


glimpse(tibble)
glimpse(roads(state, 5,2023))
