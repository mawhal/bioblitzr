library(tidyverse)
library(dplyr)

# calculate solow probabilty of presence prior used by Simon, A., Basman, A., Martin, R., Robinson, C., Cronk, Q. (2025). Operationalizing a protocol for the detection of species
# extirpation: a localized approach to a global problem. Plants, People, Planet. 
# written by Emma M 
# created: July 23/25
# last updated: July 23/25

solow_pp_prior <- function(cat, Tc, Tb, name_col, year_col, date_col) {
  
  # cat = dataframe with each row a species observation within a study region (catalogue)
  # Tc = current year (numeric, e.g. 2025)
  # Tb = baseline year (numeric, e.g. 1886), used for singleton records 
  # name_col = tidy column name (enter without quotes) representing taxon names 
  # year_col = tidy column name (enter without quotes) representing year that records were collected (optional)
  # date_col = tidy column name (enter without quotes) representing date in YYYY-MM-DD that records were collected (optional)
  # ***USER MUST INPUT EITHER year_col or date_col
  
  # defining quosures
  
  if(missing(year_col) & !missing(date_col)){ # if year column not already created
    
    # defining quosures
    date_col <- enquo(date_col)
    name_col <- enquo(name_col)
    
    # create them from date column
    cat <- cat %>% 
      mutate(year_col = substr(!!date_col, start = 1, stop = 4))
    
    # finding first record year for each taxon from catalogue
    first_year <- cat %>% 
      group_by(!!name_col) %>% 
      
      arrange(year_col, .by_group = T) %>% 
      slice(1) %>% 
      mutate(Tf = year_col) %>% 
      dplyr::select(!!name_col, Tf)
    
    # finding last record year for each taxon from catalogue
    last_year <- cat %>% 
      group_by(!!name_col) %>% 
      arrange(desc(year_col), .by_group = T) %>% 
      slice(1) %>% 
      mutate(Tl = year_col) %>% 
      dplyr::select(!!name_col, Tl)
    
    # joining and obtaining distinct taxa
    sp_list <- first_year %>% 
      left_join(last_year) %>% 
      distinct(!!name_col, .keep_all = T) 
    
    
  } else if (!missing(year_col)){ # if year column already present
    
    # defining quosures
    year_col <- enquo(year_col)
    date_col <- enquo(date_col)
    name_col <- enquo(name_col)
    
    # finding first record year for each taxon from catalogue
    first_year <- cat %>% 
      group_by(!!name_col) %>% 
      arrange(!!year_col, .by_group = T) %>% 
      slice(1) %>% 
      mutate(Tf = !!year_col) %>% 
      dplyr::select(!!name_col, Tf)
    
    # finding last record year for each taxon from catalogue
    last_year <- cat %>% 
      group_by(!!name_col) %>% 
      arrange(desc(!!year_col), .by_group = T) %>% 
      slice(1) %>% 
      mutate(Tl = !!year_col) %>% 
      dplyr::select(!!name_col, Tl)
    
    # joining and obtaining distinct taxa
    sp_list <- first_year %>% 
      left_join(last_year) %>% 
      distinct(!!name_col, .keep_all = T) 
    
  } else if (missing(year_col) & missing(date_col)){
    stop("Error: user must enter year_col or date_col")
    
  }
  
  # calculating solow pp 
  
  solow_pp <- sp_list %>% 
    # ensuring year columns read as numeric
    mutate(Tf = as.numeric(Tf), 
           Tl = as.numeric(Tl)) %>% 
    # tt = years between current year and first observation year
    # ti = years between last and first observation year 
    mutate(Tc = Tc, 
           tt = Tc-Tf, 
           ti = Tl-Tf, 
           direct_bf = case_when(ti == 0 ~ 1/(((Tc-Tb)/(Tl - Tb))-1),
                                 ti==tt ~ 1,
                                 TRUE ~ 1/((tt/ti)-1)),
           direct_solow_pp = case_when(ti == tt ~ 1, 
                                       TRUE ~ direct_bf/(1+direct_bf)))
}



