#To determine how much we need to pay out to LAs and how much we need to claw back
#Emily Keenan
#17/08/2023

#### set working directory as a shared council tax folder - wokrs for any personal drive ####
setwd("D:/Users")
dir <- list.dirs()
dir <- dir[grepl("OneDrive - MHCLG.*CT Rebate Monitoring", dir)]
dir <- sub(".","D:/Users",dir)
setwd(dir)

source("D:\\Users\\emily.keenan\\Documents\\GitHub\\ctrebate_recon\\Read_in.r")

assur_nov <- assur_nov_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, ref_core_fund, ref_discretionary_fund, sec1_a_eligible_core_value, sec2_a_disc_fund_value)                        #select columns with the inital core and dis payment and final amounts paid to eligible hoseuholds

assur_nov <- assur_nov |>
    mutate(core_difference = sec1_a_eligible_core_value - ref_core_fund, dis_difference = sec2_a_disc_fund_value - ref_discretionary_fund)

assur_nov <- assur_nov |>
    mutate( core_claw = case_when(core_difference < 0 ~ core_difference, TRUE ~ 0),
            core_payout = case_when(core_difference > 0 ~ core_difference, TRUE ~0),
            dis_claw = case_when(dis_difference < 0 ~ dis_difference, TRUE ~ 0),
            dis_writeoff = case_when(dis_difference > 0 ~ 0, TRUE ~ 0)) |>
    select(organisation_name:organisation_id, core_claw:dis_writeoff)

totals_nov <- assur_nov |>
    select(core_claw:dis_writeoff) |>
    colSums() |>
    as.data.frame()

totals_nov <- cbind(rownames(totals_nov), totals_nov)
totals_nov <- totals_nov |>
    rename( catagory = "rownames(totals_nov)",
            totals = "colSums(select(assur_nov, core_claw:dis_writeoff))")

write_csv(totals_nov, 'Assurance_June 2023\\totals_nov.csv')

recon_june <- recon_june_raw |>
    clean_names() |>
    select(organisation_name:organisation_id, status, ref_core_fund, ref_discretionary_fund, no_hholds_core_value, no_hholds_discretionary_value)                        #select columns with the inital core and dis payment and final amounts paid to eligible hoseuholds

recon_june <- recon_june |>
    mutate(core_difference = no_hholds_core_value - ref_core_fund, dis_difference = no_hholds_discretionary_value - ref_discretionary_fund)

recon_june <- recon_june |>
    mutate( core_claw = case_when(core_difference < 0 ~ core_difference, TRUE ~ 0),
            core_payout = case_when(core_difference > 0 ~ core_difference, TRUE ~0),
            dis_claw = case_when(dis_difference < 0 ~ dis_difference, TRUE ~ 0),
            dis_writeoff = case_when(dis_difference > 0 ~ 0, TRUE ~ 0)) |>
    select(organisation_name:organisation_id, core_claw:dis_writeoff)

totals_june <- recon_june |>
    select(core_claw:dis_writeoff) |>
    colSums() |>
    as.data.frame()

totals_june <- cbind(rownames(totals_june), totals_june)
totals_june <- totals_june |>
    rename( catagory = "rownames(totals_june)",
            totals = "colSums(select(recon_june, core_claw:dis_writeoff))")

write_csv(totals_june, 'Assurance_June 2023\\totals_june.csv')

total_comapre <- left_join(totals_nov, totals_june, by = "catagory") 
total_comapre <- total_comapre |>
    mutate(totals.x = abs(totals.x), 
            totals.y = abs(totals.y)) |>
    mutate(difference = totals.y - totals.x)

write_csv(total_comapre, 'Assurance_June 2023\\total_comapre.csv')


view(assur_nov)
