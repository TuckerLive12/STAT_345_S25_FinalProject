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
# Helper function for get_state_roads
#  Takes in the year and state and outputs a tibble of the lowest county FP
#  in state.
#
#
##
get_county_fips <- function(state, year) {
  
  counties <- counties(state = state, year = year) |>
    arrange(COUNTYFP)
  
  return(counties$COUNTYFP[1:5]) # REMOBER TO REMOVE [1:5]
}

##
# Takes in the year and state and outputs a tibble that has the county ID, year, ...
#
# param state_fips, a ...
# param year, 
##
get_state_roads <- function(state, year) {
  
  # Getting all the countyFIPS to get the roads in each county
  county_FIPS <- get_county_fips(state = state, year = year) 
  
  # Create a tibble with the first county's roads in it.
  #  Necessary because bind_rows() does not work if the first argument is null
  to_return <- roads(state = state, county = county_FIPS[1], year = year) |>
    
    # Drop the unnecessary and memory intensive geometry
    st_drop_geometry() |>
    mutate(
      COUNTYFP = as.character(county_FIPS[1]),
      STATE_ID = as.character(state),
      YEAR = as.character(year)  
    ) 
  
  # Go through all county fips in state, extracts the road data using roads(),
  #  and add them to tibble using bind_rows().
  for (i in 2:(length(county_FIPS))) {
    
    # (Grok) Initialize dat to NULL to avoid undefined variable and memory issues
    dat <- NULL
    dat <- roads(state = state, county = county_FIPS[i], year = year)
    
    
    # (Grok) Only proceed if dat is not NULL (Just in case)
    if (!is.null(dat)) {
      dat <- dat |>
        
        # Drop the unnecessary and memory intensive geometry
        st_drop_geometry() |>
        mutate(
          COUNTYFP = as.character(county_FIPS[i]),
          STATE_ID = as.character(state),
          YEAR = as.character(year)  
        ) 
    }
    
    # Bind the data sets together
    to_return <- bind_rows(to_return, dat)
    
  }
  
  # Return the tibble containing all the road data in the state
  return(to_return)
}

##
# Helper function for get_state_roads
#  Takes in the year and state and outputs a tibble of the lowest county FP
#  in state.
#
#
##
get_state_filepaths <- function() {
  
  # Create a tibble with state and state_fips
  state_paths <- tibble(
    # State names without white space
    state = c(
      "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
      "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
      "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
      "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "NewHampshire", 
      "NewJersey", "NewMexico", "NewYork", "NorthCarolina", "NorthDakota", "Ohio", 
      "Oklahoma", "Oregon", "Pennsylvania", "RhodeIsland", "SouthCarolina", "SouthDakota", 
      "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "WestVirginia", 
      "Wisconsin", "Wyoming"
    ),
    # State FIPS codes
    fips = c(
      "01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", "17", "18", 
      "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
      "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", 
      "48", "49", "50", "51", "53", "54", "55", "56"
    )
  )
  
  # Original file path
  original_path <- "./STAT 345/STAT_345_S25_FinalProject/Data_Scraping_and_Database/ToReplace_Roads_2023.csv"
  
  # Generate file paths and combine into a tibble
  state_paths <- state_paths |>
    rowwise() |>
    mutate(file_path = gsub("ToReplace", state, original_path)) # (Grok) Did gsub
  
  # Return the tibble
  return(state_paths)
}

##
# Helper function for get_state_roads
#  Takes in the year and state and outputs a tibble of the lowest county FP
#  in state.
#
#
##
write_all_state_roads <- function(year) {
  
  # Get file paths and State FIPS codes for extraction and writing to .csv
  States <- get_state_filepaths()
  
  
  
}


write_csv(tibble, file = "./STAT 345/STAT_345_S25_FinalProject/Data_Scraping_and_Database/ToReplace_Roads_2023.csv")

Alabama_Roads_2023 <- tibble





# Some checking commands
identical(unique(tibble$LINEARID), tibble$LINEARID)
format(object.size(tibble), units = "auto")

tibble |>
  count(STATE_ID, YEAR, COUNTY_ID)
glimpse(tibble)











## FUNCTION TO KEEP JUST IN CASE.

##
# Takes in the year and state and outputs a tibble that has the county ID, year, ...
#
# param tibble, a ...
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

