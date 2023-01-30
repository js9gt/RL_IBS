
#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

##Add source file reading in data
source('Scripts/02_data_processing.R')

## create a new column in the original variable indexing if this was a confirmed visit
## confirmed visit based on-- 
##### no conf_num (not a hospitalization)
##### no NDC code (not a pharmacy claim)
##### filtering the procedure_desc to include "office", reflecting in-office visits

## for observations in the cohort dataset that matches the above (a confirmed visit), index as 1, otherwise, 0
## we also don't want anything with an NDC code, which is a prescription-- we want NDC code to return NA
cohort <- cohort %>% mutate(visit_event = ifelse( is.na(conf_num) & 
                                                        ndc== "" & 
                                                        grepl("office", procedure_desc, ignore.case = TRUE) , 1, 0))

# create outpatient dataset which doesn't include hospitalizations or prescriptions-- need to have an NA for NDC code
outpatient <- cohort %>% filter(is.na(conf_num) & ndc == "")


