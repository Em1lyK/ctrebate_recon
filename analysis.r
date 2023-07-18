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
assur_loc <- 'https://raw.githubusercontent.com/Em1lyK/ctrebate_recon/main/Input/council-tax-energy-rebate-assurance_20230711082613.csv?token=GHSAT0AAAAAACEIZD5TPGZDNMZUNYACKKUEZFPWZXA'
recon_loc <- 'https://raw.githubusercontent.com/Em1lyK/ctrebate_recon/main/Input/council-tax-energy-rebate-reconciliation_20230718093125.csv'

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
    select(organisation_name:organisation_id, sec1_a_eligible_core, sec1_a_eligible_core_value, sec1_b_pay_ineligible_value, sec1_b_ineligible_recover_value, sec2_a_disc_pay_no_hh, sec2_b_disc_ineligible, sec2_b_disc_ineligible_recover_value)

recon_june <- recon_june_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, status, no_hholds_core, no_hholds_core_value, no_hholds_core_value_ineligible, no_hholds_core_value_ineligible_recover, no_hholds_discretionary, no_hholds_discretionary_ineligible, no_hholds_discretionary_value_recovered) |>
    filter(!no_hholds_core == 'Invalid Number') |>
    filter(!status == 'saved')

#reasons for changes 
june_reason <- recon_june_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, status, no_hholds_core_v, no_hholds_core_value_ineligible_v, no_hholds_core_value_ineligible_recover_v, no_hholds_discretionary_v , no_hholds_discretionary_ineligible_v) |>
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

#write.csv(elig_hhcore, "Output\\las_corehh_inc.csv")                                                                                #output number of LAs with core eligible household inc
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
    mutate(inel_hh_change = inel_core_diff/150) |>
    mutate(june_inel_vale_divide150 = no_hholds_core_value_ineligible/150)
inel_core_change <- comparision_core_inel_df |>                                                                                     #isolate las with a chnage in number from nov to June
    filter(inel_core_diff != 0)

inel_core_change <- inel_core_change |>
    select(!nov_core_hh_inel:no_hholds_core_value_ineligible) |>                                                                    #remove uncecessary columns                                                                                   #calculate the number of houeholds equivalents to the ineligible households
    left_join(select(june_reason, organisation_id, no_hholds_core_value_ineligible_v), by = 'organisation_id')                      #join on the reasons for an inreligible change in June
#write.csv(inel_core_change, "Output\\las_coreinel_dec.csv")                                                                         #output number of LAs with core eligible household inc

###create a data frame with the change in eligible and ineligible payments next to eachother 
inel_comp_el_inc <- full_join(ons_names, select(comparision_core_inel_df, organisation_id, inel_hh_change), by = 'organisation_id')     #attached the difference in the number of ineligible households in nov and june to ons codes for all 2022/23 las
inel_comp_el_inc <- full_join(inel_comp_el_inc, select(comparision_core_el_df, organisation_id, el_hh_change), by = 'organisation_id')  #attached the difference in the number of eligible households in nov and june 
inel_comp_el_inc <- inel_comp_el_inc |>
    mutate(comparision = el_hh_change + inel_hh_change) |>                                                                              #add the diff in the number of eligible and ineligible hh to see if it is net zero (eligible hh has gone down by the number of ineligible hh gone up)
    na.omit() |>
    arrange(desc(comparision))                                                                                                                           #remove las who have not returned to make sorting easier 
inel_comp_el_inc <- left_join(inel_comp_el_inc, select(june_reason, organisation_id, no_hholds_core_v, no_hholds_core_value_ineligible_v), by = 'organisation_id')  #add the reasona for any change in numbers
inel_comp_el_inc <- left_join(inel_comp_el_inc
#write.csv(inel_comp_el_inc, "Output\\change_el_inel_comparision.csv")                                                                   #write comparision to a csv file
view(inel_comp_el_inc)
#write.csv(inel_comp_el_inc, 'Output/compare_changes_inel_el_core.csv')

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

#write.csv(las_recov_core, "Output\\las_core_recov_dec.csv")                                                                                          #output number of LAs with core eligible household inc

####################################################################################################################################
############## DISCRETIONARY SCHEME ###################################################
####################################################################################################################################

### Change in the number of eligible payments ###
change_eligible_dis <- assur_nov |>
    select(organisation_name:organisation_id, sec2_a_disc_pay_no_hh) |>                                                     #select the number of disc eligible hh in Nov
    rename(nov_disc_el_hh = sec2_a_disc_pay_no_hh)                                                                          #rename to specify nov
change_eligible_dis <- change_eligible_dis |>
    left_join(select(recon_june, organisation_id, no_hholds_discretionary), by = 'organisation_id') |>                      #select number of disc eligible hh in june
    rename(june_disc_el_hh = no_hholds_discretionary) |>                                                                    #rename to specify june
    mutate(change_disc_el_hh = june_disc_el_hh - nov_disc_el_hh)                                                            #calc the diff between june and nov el hh

view(change_eligible_dis)

### Change in the number of ineligible payments ###
change_ineligible_dis <- assur_nov |>
    select(organisation_name:organisation_id, sec2_b_disc_ineligible) |>                                                    #select the number of disc ineligible hh in Nov
    rename(nov_disc_inel_hh = sec2_b_disc_ineligible)                                                                       #rename to specify nov
change_ineligible_dis <- change_ineligible_dis |>
    left_join(select(recon_june, organisation_id, no_hholds_discretionary_ineligible), by = 'organisation_id') |>           #select number of disc ineligible hh in june
    rename(june_disc_inel_hh = no_hholds_discretionary_ineligible) |>                                                       #rename to specify june
    mutate(change_disc_inel_hh = june_disc_inel_hh - nov_disc_inel_hh)                                                      #calc the diff between june and nov inel hh

view(change_ineligible_dis)

###compare changes in ineligible and eligible households discretionary ###
compare_changes_inel_el_dis <- change_ineligible_dis |>
    select(organisation_name:organisation_id, change_disc_inel_hh)                                                          #select change in the number of inel hh column
compare_changes_inel_el_dis <- compare_changes_inel_el_dis |>
    left_join(select(change_eligible_dis, organisation_id, change_disc_el_hh), by = 'organisation_id') |>                   #select change in the number of el hh column
    mutate(net_change_hh = change_disc_inel_hh + change_disc_el_hh)                                                         #add the to columns to see the net change in the number of hh paid 

compare_changes_inel_el_dis <- compare_changes_inel_el_dis |>
    left_join(select(june_reason, organisation_id, no_hholds_discretionary_v , no_hholds_discretionary_ineligible_v), by = 'organisation_id')       #Attached the reasons for changes in the n.o. of el and inel hh
view(compare_changes_inel_el_dis)
#write.csv(compare_changes_inel_el_dis, "Output/compare_changes_inel_el_dis.csv")
#################################################
########  check disisible by 150 ################
#################################################
####eligible
divisible_150_el <- recon_june |>
    select(organisation_name:organisation_id, no_hholds_core_value) |>                                                      #Select value of core payments in June
    mutate(divide_150_el = no_hholds_core_value/150)                                                                        #divide the value of core payments by 150

remainder_150 <- divisible_150_el |>
    filter(!no_hholds_core_value %% 150 == 0)                                                                               #filter core payments column by value of payments that are not divisible by 150 
view(remainder_150)
#write.csv(remainder_150, file = "Output/remainder_150.csv")

#ineligible
divisible_150_inel <- recon_june |>
    select(organisation_name:organisation_id, no_hholds_core_value_ineligible) |>
    mutate(divide_150_inel = no_hholds_core_value_ineligible/150)

remainder_150 <- divisible_150_inel |>
    filter(!no_hholds_core_value_ineligible %% 150 == 0)
view(remainder_150)



#### compared with validations log ####
validations <- read_excel('Input/230621 - validations logs_ek.xlsx', sheet = 'Reconciliation issues log')       #read in validations log
view(validations)
