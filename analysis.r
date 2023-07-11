#Analysis of CT rebate reconciliation data and comparision with assurance data 
#Emily Keenan
#11/07/2023

#libraries
library(tidyverse)
library(readxl)
library(janitor)

#working directory to save things 
setwd("D:\\Users\\emily.keenan\\Documents\\GitHub\\ctrebate_recon")

#input locations in github
assur_loc <- 'https://raw.githubusercontent.com/Em1lyK/ctrebate_recon/main/Input/council-tax-energy-rebate-assurance_20230711082613.csv?token=GHSAT0AAAAAACEIZD5SIVP2WTGPUR7K7O5GZFNDHQA'
recon_loc <- 'https://raw.githubusercontent.com/Em1lyK/ctrebate_recon/main/Input/council-tax-energy-rebate-reconciliation_20230711082839.csv?token=GHSAT0AAAAAACEIZD5TDAU5O55F34IS23Q2ZFNDIPQ'

incomplete <- c('saved')

#read in June 2023 reconciliation data
assur_nov_raw <- read_csv(assur_loc)
recon_june_raw <- read_csv(recon_loc)

##########################################
#clean data names and select:
#########################################
#id columns
#number of eligible household paid core 
#value of core payments made to eligible households
#total value of ineligible payments 
#total value of recovered ineligible payments

assur_nov <- assur_nov_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, sec1_a_eligible_core, sec1_a_eligible_core_value, sec1_b_pay_ineligible_value, sec1_b_ineligible_recover_value)

recon_june <- recon_june_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, status, no_hholds_core, no_hholds_core_value, no_hholds_core_value_ineligible, no_hholds_core_value_ineligible_recover) |>
    filter(!no_hholds_core == 'Invalid Number') |>
    filter(!status == 'saved')

#reasons for changes 
june_reason <- recon_june_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, status, no_hholds_core_v, no_hholds_core_value_ineligible_v, no_hholds_core_value_ineligible_recover_v) |>
    filter(!status == 'saved')

#data frame with all ons codes and la names 
ons_names <- assur_nov |>
    select(organisation_name:organisation_id)

####compare eligible households core payments#### 
comparision_core_el_df <- assur_nov |>
    select(organisation_name:organisation_id, sec1_a_eligible_core, sec1_a_eligible_core_value) |>                                  #select eligible core columns
    rename(nov_core_hh_el = sec1_a_eligible_core, nov_core_val_el = sec1_a_eligible_core_value)                                     #rename column titles to specify the nov collection
comparision_core_el_df <- comparision_core_el_df |>
    left_join(select(recon_june, organisation_id, no_hholds_core, no_hholds_core_value) , by = 'organisation_id') |>                #join the June data 
    na.omit()
comparision_core_el_df <- comparision_core_el_df |>
    mutate(el_hh_change = no_hholds_core - nov_core_hh_el)                                                                            #number of households paid core in June - number of households paid cor in niv

elig_hhcore <- comparision_core_el_df |>                                                                                            #data frame of las with changes to core eligible hh paid in june compared to Nov
    filter(el_hh_change != 0) |>
    select(!nov_core_hh_el:no_hholds_core_value) |>                                                                                 #remove unecessary data
    left_join(select(june_reason, organisation_id, no_hholds_core_v), by = 'organisation_id')                                       #join the reasons for a number change

write.csv(elig_hhcore, "Output\\las_corehh_inc.csv")                                                                                #output number of LAs with core eligible household inc
view(elig_hhcore)

###count the number of las that increased eligible payments 
nrow(elig_hhcore |>
        filter(el_hh_change > 0))

####compare ineligible payments####
comparision_core_inel_df <- assur_nov |>
    select(organisation_name:organisation_id, sec1_b_pay_ineligible_value) |>                                                       #select eligible core columns
    rename(nov_core_hh_inel = sec1_b_pay_ineligible_value)                                                                          #rename column titles to specify the nov collection
comparision_core_inel_df <- comparision_core_inel_df |>
    left_join(select(recon_june, organisation_id, no_hholds_core_value_ineligible) , by = 'organisation_id') |>                     #joing the june ineligible value data
    na.omit()
comparision_core_inel_df <- comparision_core_inel_df |>
    mutate(inel_core_diff = no_hholds_core_value_ineligible - nov_core_hh_inel) |>                                                     #June value ineligible payments minus nov ineligible payments 
    mutate(inel_hh_change = inel_core_diff/150)
inel_core_change <- comparision_core_inel_df |>                                                                                     #isolate las with a chnage in number from nov to June
    filter(inel_core_diff != 0)

inel_core_change <- inel_core_change |>
    select(!nov_core_hh_inel:no_hholds_core_value_ineligible) |>                                                                    #remove uncecessary columns                                                                                   #calculate the number of houeholds equivalents to the ineligible households
    left_join(select(june_reason, organisation_id, no_hholds_core_value_ineligible_v), by = 'organisation_id')                      #join on the reasons for an inreligible change in June
write.csv(inel_core_change, "Output\\las_coreinel_dec.csv")                                                                         #output number of LAs with core eligible household inc

###create a data frame with the change in eligible and ineligible payments next to eachother 
inel_comp_el_inc <- full_join(ons_names, select(comparision_core_inel_df, organisation_id, inel_hh_change), by = 'organisation_id')
inel_comp_el_inc <- full_join(inel_comp_el_inc, select(comparision_core_el_df, organisation_id, el_hh_change), by = 'organisation_id')
inel_comp_el_inc <- inel_comp_el_inc |>
    mutate(comparision = el_hh_change + inel_hh_change) 
write.csv(inel_comp_el_inc, "Output\\change_el_inel_comparision.csv")   

inel_dec_corr_el_inc <- full_join(inel_core_change, elig_hhcore, by = "organisation_id")                                            #
inel_dec_corr_el_inc <- inel_dec_corr_el_inc |>
    dplyr::select(!organisation_name.y, !inel_core_diff) |>
    relocate(el_hh_change, .after = organisation_id) |>
    mutate(comparision = el_hh_change + inel_hh_change) |>
    relocate(comparision, .after = inel_hh_change) |>
    arrange(inel_hh_change)
view(inel_dec_corr_el_inc)
write.csv(inel_dec_corr_el_inc, "Output\\change_el_inel_comparision.csv")   
view(inel_core_change)
view(inel_comp_el_inc)

####compare recovered payments####
comparision_core_recov_df <- assur_nov |>
    select(organisation_name:organisation_id, sec1_b_ineligible_recover_value) |>                                  #select eligible core columns
    rename(nov_core_hh_recov = sec1_b_ineligible_recover_value)
comparision_core_recov_df <- comparision_core_recov_df |>
    left_join(select(recon_june, organisation_id, no_hholds_core_value_ineligible_recover) , by = 'organisation_id') |>
    na.omit()
comparision_core_recov_df <- comparision_core_recov_df |>
    mutate(recov_core_diff = no_hholds_core_value_ineligible_recover - nov_core_hh_recov) 

nrow(las_recov_core <- comparision_core_recov_df |>                                                                                                      #number of LAs with core eligible households paid since Nov increase
filter(recov_core_diff < 0))

las_recov_core <- las_recov_core |>
    select(!nov_core_hh_recov:no_hholds_core_value_ineligible_recover) |>
    mutate(hh_equ_recov = recov_core_diff/150) |>
    left_join(select(june_reason, organisation_id, no_hholds_core_value_ineligible_recover_v), by = 'organisation_id')


write.csv(las_recov_core, "Output\\las_core_recov_dec.csv")                                                                                          #output number of LAs with core eligible household inc

inel_dec_corr_el_inc <- left_join(las_inel_core, select(las_el_hhcore, !nov_core_hh_el:no_hholds_core), by = "organisation_id")


str(recon_june_raw)
view(recon_june_raw)
nrow(comparision_core_el_df)
view(las_recov_core)
view(recon_june)
view(las_el_hhcore)
