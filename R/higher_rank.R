# function to populate higher ranks (family, order, class, phylum, kingdom) from taxon name at the species level using the taxadb pacakge
# written by Emma M 
# created: July 23/25
# last updated: July 23/25

# could be streamlined in the future to identify rows where this information is missing and only query for those rows rather than checking each row

library(tidyverse)
library(dplyr)
library(taxadb)

higher_rank <- function(df, names, database){
  # df = dataframe. One of either a catalogue of species occurrence records (each row is an observation) Or a species list (each row is a distinct taxon)
  # names = column name containing taxon names to species level as tidy column name (no quotations)
  # database = character specifying database options by the taxadb package to query taxon information from. Default - "gbif" 
  
  # specifying database type if missing
  if(missing(database)){
    database = "gbif"
  }
  
  # defining quosures
  names <- enquo(names)
  
  if(dim(df)[1]==0){
    return(df)
    
  } else {
    
    #create local taxonomy database
    td_create(database, overwrite=T)
    
    df <- df %>% 
      # ensuring names are read to species level (not variety or subspecies level)
      separate(!!names, into = c("genus", "species", "intraspecific_designation", "intraspecific_epithet"), sep = " ",
               remove = F) %>% 
      # combining genus and species, when hybrid symbol present, including string separated as intraspecific_designation to get hybrid genus and specific epithet
      mutate(species = case_when(str_detect(!!names, pattern = "×") ~ paste(genus, species, intraspecific_designation), 
                                 !str_detect(!!names, pattern = "×") ~ paste(genus, species))) %>% 
      dplyr::select(-intraspecific_designation, -intraspecific_epithet)
    
    if(!"family" %in% colnames(df)){ 
      df$family <- ""
    } 
    
    if (!"order" %in% colnames(df)){
      df$order <- ""
    } 
    
    if (!"class" %in% colnames(df)){
      df$class <- ""
    } 
    
    if (!"phylum" %in% colnames(df)){
      df$phylum <- ""
    }
    
     if (!"kingdom" %in% colnames(df)){
      df$kingdom <- ""
    }
      
    ## querying species names from database
    df_missing <- df %>% 
      filter(family == "" | is.na(family) | 
               order == "" | is.na(order) | 
               class == "" | is.na(class) | 
               phylum == "" | is.na(phylum)| 
               kingdom == "" | is.na(kingdom))
    
    # initializing empty dataframe
      temp <- df_missing %>% 
          ungroup() %>% 
          dplyr::select(!!names, kingdom, phylum, class, order, family)         
            
             
    for(i in 1:dim(df_missing)[1]){ # for each row of the dataframe
      
      # query the species name to the database 
       tmp <- df_missing %>% 
        ungroup() %>% 
        dplyr::select(!!names) %>% 
        filter(row_number(!!names) == i) %>% 
        as.character() %>% 
        filter_name(., database)
        
      if(dim(tmp)[1] == 0){
        temp[i, 2:6] <- NA
        
      } else if (dim(tmp)[1] > 0) {
        temp[i, 2:6] <- tmp %>% 
          arrange(taxonomicStatus) %>% 
          slice(1) %>% 
          # select best match information
          dplyr::select(kingdom, phylum, class, order, family) %>% 
          data.frame()
      }
      
    }
     
    ## assigning missing information on kingdom, phylum, class, order, family when missing from original dataframe
    for (i in 1:dim(df)[1]){
      if(any(as.character(slice(ungroup(df), i) %>% dplyr::select(!!names)) == 
             dplyr::select(temp, !!names))){
        df$kingdom[i] <- temp$kingdom[i]
        df$phylum[i] <- temp$phylum[i]
        df$class[i] <- temp$class[i]
        df$order[i] <- temp$order[i]
        df$family[i] <- temp$family[i]
      }
    }
  }
  
  df <- df %>% 
    dplyr::select(!!names, genus, family, order, class, phylum, kingdom)
  
  return(df)
}
