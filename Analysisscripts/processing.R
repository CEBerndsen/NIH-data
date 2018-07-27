# load packages
library(tidyverse)
library(readxl)
library(tools)

# get file list and names
files <- dir(pattern = "*.xlsx")

# make function to trim and combine data sets into single dataframe

NIH_state_year <- function(x){
  #load dataset
  df <- read_excel(x)
  
  year <- file_path_sans_ext(x)
  
  #trim data and remove non-US countries
  df_tot <- df %>%
    filter(!is.na(STATE)) %>%
    filter(COUNTRY == "UNITED STATES") %>%
    group_by(STATE) %>%
    # summarize the dollars and number of awards by state
    summarise(fund_tot = sum(FUNDING), numaward_tot = sum(AWARDS)) %>%
    #add a year column
    mutate(yr = year)
  
}

# Use map_dfr to run over all the datasets and combine them into a single dataframe
NIH_annual <- map_dfr(files, NIH_state_year)

NIH_year <- function(x){
  #load dataset
  df <- read_excel(x)
  
  year <- file_path_sans_ext(x)
  
  #trim data and remove non-US countries
  df_tot <- df %>%
    # remove non-north american countries
    filter(!is.na(STATE)) %>%
    # keep to only US states
    filter(COUNTRY == "UNITED STATES") %>%
    #add a year column
    mutate(yr = year)
  
}

NIH_US_year <- map_dfr(files, NIH_year)


NIH_fund <- function(x){
  #load dataset
  df <- read_excel(x)
  
  year <- file_path_sans_ext(x)
  
  #trim data and remove non-US countries
  df_tot <- df %>%
    #add a year column
    mutate(yr = year)
  
}

NIH_fund_year <- map_dfr(files, NIH_fund)
