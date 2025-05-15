library(tidyverse)
library(foreign)
library(tigris)
library(usethis) # GitHub
library(gitcreds) # GitHub
library(sf)

################################################
### TO DO ###
################################################
# Run write_all_state_roads() for all the data (Make sure there are no contraints)
################################################
################################################

###########
# Function summary comments done by Grok with minor edits
###########

#' Get County FIPS Codes for a State
#'
#' Retrieves the FIPS codes for counties in a specified state and year.
#'
#' @param state The state for which to retrieve county FIPS codes (numeric or character).
#' @param year The year for the data (numeric or character).
#' @return A character vector of county FIPS codes.
get_county_fips <- function(state, year) {
  
  counties <- counties(state = state, year = year) |>
    arrange(COUNTYFP)
  
  return(counties$COUNTYFP) 
}

#' Retrieve Road Data for a State
#'
#' Downloads road data for all counties in a specified state and year, combining them into a single tibble.
#'
#' @param state The state for which to retrieve road data (numeric or character, typically a FIPS code).
#' @param year The year for the road data (numeric or character).
#' @return A tibble containing road data for the specified state, with columns for LINEARID, FULLNAME, RTTYP, 
#'  MTFCC, COUNTYFP, STATE_ID, and YEAR.
get_state_roads <- function(state, year) {
  
  # Getting all the countyFIPS to get the roads in each county
  county_FIPS <- get_county_fips(state = state, year = year) 
  
  # Helpful printout
  print(paste("Downloading state", state, "county", county_FIPS[1]))
  
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
    
    # Helpful printout
    print(paste("Downloading state", state, "county", county_FIPS[i]))
    
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

#' Generate File Paths for State Road Data
#'
#' Creates a tibble with state names, FIPS codes, and corresponding file paths for road data files.
#'
#' @param filepath A character string representing the file path template, with "ToReplace" as a placeholder for state names.
#' @return A tibble with columns: state (state names), fips (state FIPS codes), and file_path (file paths for road data).
get_state_filepaths <- function(filepath) {
  
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
  original_path <- filepath
  
  # Generate file paths and combine into a tibble
  state_paths <- state_paths |>
    rowwise() |>
    mutate(file_path = gsub("ToReplace", state, original_path)) # (Grok) Did gsub
  
  # Return the tibble
  return(state_paths)
}

#' Write Road Data for All States to CSV Files
#'
#' Downloads road data for specified states and writes it to separate CSV files, managing memory by removing data after writing.
#'  Downloads in acceding order.
#'
#' @param filepath A character string representing the file path template for saving road data, with "ToReplace" as a placeholder (e.g., "./Data_Scraping_and_Database/ToReplace_Roads_2023.csv").
#' @param year The year for the road data (numeric or character).
#' @param states A numeric vector of state indices (1 to 50) or 0 to download all states. Default is 0.
#' @return None. The function writes CSV files to the specified file paths.
write_state_roads <- function(filepath, year, states = 0) {
  
  # Check if staes is asingle vector (to check next if statement)
  if (length(states) == 1) {
    # Check if states = 0, if so download all states (1:50)
    if (states == 0) {
      states = 1:50
    }
  }  
  
  # Get file paths and State FIPS codes for extraction and writing to .csv
  statesFIPS_filepath <- get_state_filepaths(filepath)
  
  # Go through all states (FIPS codes) and extracts the road data
  #  then it writes that road data to .csv and removes it from memory
  for(i in 1:(length(states))) { 
    
    # Gets a tibble of all the roads in that state
    tibble <- get_state_roads(statesFIPS_filepath$fips[states[i]], year = year)
    
    # writes that tibble to the folder with the file_path column
    write_csv(tibble, file = statesFIPS_filepath$file_path[states[i]])
    
    # Removes tibble from memory
    rm(tibble)
    
    # Clearing unused memory
    gc()
    
    # Helpful printout
    print(paste0("Done writing state ", states[i]))
  }
  
  # A nice printout
  print("Done Downloading!")
}

# Downloaded 50 state's roads
#write_state_roads("./Data_Scraping_and_Database/ToReplace_Roads_2023.csv",2023,0)






# Some checking commands
#identical(unique(tibble$LINEARID), tibble$LINEARID)
#format(object.size(tibble), units = "auto")

#tibble |>
#  count(STATE_ID, YEAR, COUNTY_ID)
#glimpse(tibble)









