#Read in assurance data (from June) and reconciliation data (From Decemeber)
#Emily Keenan
#17/08/2023

#libraries
library(tidyverse)
library(readxl)
library(janitor)
 
#### set working directory as a shared council tax folder - wokrs for any personal drive ####
setwd("D:/Users")
dir <- list.dirs()
dir <- dir[grepl("OneDrive - MHCLG.*CT Rebate Monitoring", dir)]
dir <- sub(".","D:/Users",dir)
setwd(dir)

#input locations in github
assur_loc <- 'Assurance_June 2023\\council-tax-energy-rebate-assurance_20230817082843.csv'
recon_loc <- 'Reconciliation\\council-tax-energy-rebate-reconciliation_20230731142255.csv'

incomplete <- c('saved')

#read in June 2023 reconciliation data
assur_nov_raw <- read_csv(assur_loc)
recon_june_raw <- read_csv(recon_loc)
