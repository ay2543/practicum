#####################################################################################################
# Title: Supplementary R Code - Data preparation and merging.R                                      #
# Authors: Luis Segura and Natalie Levy                                                             #
# Purpose: Prepare NSDUH data for analytic examples                                                 #
#####################################################################################################

############################################
#                                          #
# Load and prepare the NSDUH raw datasets  #
#                                          #
############################################

# Clear all objects from environment
rm(list = ls())

# Load needed packages
library(readstata13)
library(tidyverse)

### Set the directory where the datasets are stored
### Raw data are available in the course Dropbox folder
### You will need to update the filepath below to the appropriate location on your computer
setwd("/Users/amyyeung/Dropbox/Practicum/nsduh") 


### assign the names of the Stata datasets to a vector
nsduh_data_names <- list.files() 
str(nsduh_data_names)


### create an empty list where we will store all the datasets as we read them in
nsduh_list <- list()


### Create a vector of the variable names we want to keep
var_names <- c("caseid", "realid", "vestr", "verep", "year", "heryr", "anlyr", "mrjyr", "newrace2", 
               "iranlfy", "age2", "income", "irsex", "pden", "abodanl", "abodher", "anydrug", "analwt_c",
               
               "txyrndalc", "txyrndill", "txyrndilal", #need tx under dsm-iv
               "ndfltxalc", "ndfltxill", "ndfltxilal", #felt needed tx
               "txyralc", "txyrill", "txyralnil", "txyrilnal", "txyrrecvd2", #received tx at any location
               "txpdhinsal", "txpdmcreal", "txpdmcadal", "txpdpublal", "txpdsvngal",
               "txpdhinsil", "txpdmcreil", "txpdmcadil", "txpdpublil", "txpdsvngil",
               "txpayhins2", "txpaymcre2", "txpaymcad2", "txpaypubl2", "txpaysvng2", #payment for tx
               "txyrhosal", "txyrresal", "txyroutal", "txyrmhcal", "txyremral",
               "txyrhosil", "txyrresil", "txyroutil", "txyrmhcil", "txyremril", 
               "txyrhosov2", "txyrresov2", "txyroutpt2", "txyrmhcop2", "txyremrgn2")


### loop to import the 8 NSDUH datasets into the list we created ("nsduh_list")

for(i in seq_along(nsduh_data_names)){
  
  # Read each dataset into an element of the list nsduh_list
  nsduh_list[[nsduh_data_names[i]]] <- read.dta13(nsduh_data_names[i],
                                                   convert.factors = T,
                                                   generate.factors = T,
                                                   nonint.factors = T)
  
  # Convert variable names to lowercase
  names(nsduh_list[[i]]) <- tolower(names(nsduh_list[[i]]))
  
  # Create year variable
  nsduh_list[[i]]$year <- if_else(i<5, 2014+i, 2016+i)
  
  # Create realid variable to distinguish between observations from different years
  nsduh_list[[i]]$realid <- (nsduh_list[[i]]$year * 1e5) + nsduh_list[[i]]$caseid
  
  # Restrict datasets to just the variables we need for the analysis
  nsduh_list[[i]] <- nsduh_list[[i]][, names(nsduh_list[[i]]) %in% var_names]
}

### Append all of the list elements into a single dataframe
nsduh <- do.call(rbind, nsduh_list)

### Save data

save(nsduh, file = "nsduh.RData")
save(nsduh, file = "nsduh.csv")
