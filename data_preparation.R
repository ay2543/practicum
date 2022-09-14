#####################################################################################################
# Title: Practicum data preparation                                                                 #
# Authors: Luis Segura                                                                              #
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
library(tidyverse)

### I downloaded the R data files from SAMHSA from here: https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2020-nsduh-2020-ds0001
### and copied the dropbox link of each dataset into a vector.
dataURL <- c("https://www.dropbox.com/s/3ivrbxv9qvioscs/NSDUH_2015.RData?dl=0", 
                      "https://www.dropbox.com/s/obm4s882e1maeoa/NSDUH_2016.RData?dl=0", 
                      "https://www.dropbox.com/s/3lhrru6mrbfxpi1/NSDUH_2017.RData?dl=0", 
                      "https://www.dropbox.com/s/5dhwevv1it9fl0z/NSDUH_2018.RData?dl=0", 
                      "https://www.dropbox.com/s/2w239nt11or5u61/NSDUH_2019.RData?dl=0", 
                      "https://www.dropbox.com/s/4fz9n4w9b8bmemk/NSDUH_2020.Rdata?dl=0")

### To load datasets directly from dropbox, you have to change the dl-0 at the end for dl=1 and then load datasets into the R environment 
### using the load() function and the url() function. The latter tells R that you are loading something from an URL
for(i in dataURL){
  str_sub(i, -1, -1) <- "1"
  load(url(i))
}

## Unify variable names
NSDUH_2020$ANALWT_C <- NSDUH_2020$ANALWTQ1Q4_C
NSDUH_2020$vestr <- NSDUH_2020$VESTRQ1Q4_C

ls(pattern = "PUF") ### look for the dataset names, they start with PUF

### save all datasets into a list
dataList <- list(NSDUH15 = PUF2015_021518, 
                 NSDUH16 = PUF2016_022818, 
                 NSDUH17 = PUF2017_100918, 
                 NSDUH18 = PUF2018_100819, 
                 NSDUH19 = PUF2019_100920, 
                 NSDUH20 = NSDUH_2020) 

### Create a vector of the variable names we want to keep
var_names <- c("txyrndalc", "txyrndill", "txyrndilal", #need tx under dsm-iv
               "ndfltxalc", "ndfltxill", "ndfltxilal", #felt needed tx
               "txyralc", "txyrill", "txyralnil", "txyrilnal", "txyrrecvd2", #received tx at any location
               "txpdhinsal", "txpdmcreal", "txpdmcadal", "txpdpublal", "txpdsvngal",
               "txpdhinsil", "txpdmcreil", "txpdmcadil", "txpdpublil", "txpdsvngil",
               "txpayhins2", "txpaymcre2", "txpaymcad2", "txpaypubl2", "txpaysvng2", #payment for tx
               "txyrhosal", "txyrresal", "txyroutal", "txyrmhcal", "txyremral",
               "txyrhosil", "txyrresil", "txyroutil", "txyrmhcil", "txyremril", 
               "txyrhosov2", "txyrresov2", "txyroutpt2", "txyrmhcop2", "txyremrgn2",
               "amhtxrc3", #received any MH trx in the past yr
               "spdmon", #psychological distress indicator via RC-K6 score
               
               "catage", "irsex", "newrace2", "income", "analwt_c", "vestr", "verep")

### do some data wrangling
for(i in 1:6){
  names(dataList[[i]]) <- tolower(names(dataList[[i]])) ### turning variable names to low caps
  dataList[[i]] <- dataList[[i]][ , var_names] ### subsetting each dataset with only the variables in myvars
  dataList[[i]][["year"]] <- i + 2014 ### creating a year variable (starting year - 1)
  dataList[[i]][["newid"]] <- seq_len(nrow(dataList[[i]])) ### creating a numeric variable 1 to the length of the dataset.
  dataList[[i]][["realid"]] <- (dataList[[i]][["year"]] * 1e5) + as.numeric(dataList[[i]][["newid"]]) ### creating a unique id for each observation
}

### merge datasets together into one object called nsduh
nsduh <- dataList %>%
  bind_rows()

### some cleanup
rm("PUF2015_021518", "PUF2016_022818", "PUF2017_100918", "PUF2018_100819", "PUF2019_100920", "NSDUH_2020", "dataList") ### remove extra stuff from the environment

### Set the directory where you want to save the final dataset 
setwd("/Users/luissegura/Dropbox/Practicum (1)/nsduh") 

save(nsduh, file = "nsduh.RData")
save(nsduh, file = "nsduh.csv")
