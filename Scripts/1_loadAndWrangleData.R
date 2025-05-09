library(here)
library(tidyverse)
library(magrittr)
library(stringr)

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
options(OutDec = ",")
options(scipen=999)
options(dplyr.summarise.inform = FALSE)


### 1a. Load and merge data files ###
data_file_path <- list.files(
  here("Data", "MMS_data"), 
  pattern = ".rdata", 
  full.names = T)

#data_file_path
#ff <- data_file_path[1]

# read csv files
data_read <- data_file_path %>% 
  map_df(function(ff) {
    temp_env <- new.env()
    
    # Load the file into the temporary environment
    load(ff, envir = temp_env)
    
    # Get the name of the loaded object (assuming there's only one)
    df_name <- ls(temp_env)[1]
    
    # Access the data frame using the detected name
    rr <- get(df_name, envir = temp_env)

    cyear <- str_extract(colnames(rr), "^W\\d{2}") %>% enframe %>% 
      count(value) %>% arrange(desc(n)) %>% 
      slice(1) %>% .$value 
    
    year <- str_c("20", str_remove(cyear, "^W")) %>% as.numeric() 
    
    colnames(rr) <- str_remove_all(colnames(rr), "^W\\d{2}")  
    idx_weight<- grepl("^weight_", colnames(rr)) %>% which()
    
    stopifnot(length(idx_weight) == 1)
    colnames(rr)[idx_weight] <- "weight"

    rr %>% 
     # select(matches("^[A-Z][0-9]+$"), weight) %>% 
      mutate(year = year) %>% 
      as_tibble() 
  }) 

# save csv files

data_read %>% 
  select(matches("^[A-Z][0-9]+$"), year, weight) %>% 
  write_csv(here("Output", "data", "merged_MMS_AWYonly_files.csv"))


data_read %>% 
  write_csv(here("Output", "data", "merged_MMS_complete_files.csv"))
