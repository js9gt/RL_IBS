

## Reading in data

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("~/RL_IBS")

##Add source file reading in data
source('Scripts/02_data_processing.R')


## ---------------------- defining an event (resection) --------------------##
########################## a patient experiences an event = bowel resection when the groups are the following 
########### group:
##### = large_bowel
##### = small_bowel
##### = ileocecal
########################## reference these according to the "IQVIA Analysis Components" Documents
########## We want to create a column of events with a running total of these through time.
########## once the second event is reached, we censor. We also want to alter, so that our baseline visit (visit 0) is the first resection


cohort <- cohort %>% mutate(resection = ifelse(proc_cde ==  "44140" |  ### large bowel
                                       proc_cde == "44141"| 
                                       proc_cde == "44143" | 
                                       proc_cde == "44144"|
                                       proc_cde == "44145" |
                                       proc_cde == "44146"|
                                       proc_cde == "44147" |
                                       proc_cde == "44150"|
                                       proc_cde == "44151" |
                                       proc_cde == "44155"|
                                       proc_cde == "44156" |
                                       proc_cde == "44157"|
                                       proc_cde == "44158" |
                                       proc_cde == "44204"|
                                       proc_cde == "44206" |
                                       proc_cde == "44207"|
                                       proc_cde == "44208" |
                                       proc_cde == "44210"|
                                       proc_cde == "44211" |
                                       proc_cde == "44212" |
                                       proc_cde ==  "44120" | ## small bowel
                                       proc_cde == "44121"| 
                                       proc_cde == "44125" | 
                                       proc_cde == "44202" |
                                       proc_cde ==  "44160" | ## ileo-cecal
                                       proc_cde == "44205" , 1, 0)) %>%
  
  group_by(pat_id) %>% arrange(from_dt, desc(resection)) %>%
  
  
# Identify the last resection, considering only the date if conf_num is missing
mutate(last_resection = if_else(lag(resection) == 1, 
                                if_else(!is.na(lag(conf_num)), lag(conf_num), as.character(lag(from_dt))), 
                                NA_character_)) %>%
  tidyr::fill(last_resection) %>%
  
  # Create an indicator for unique resections, considering the date when conf_num is missing
  mutate(unique_resc_indicator = ifelse(
    resection == 1 & (is.na(last_resection) | 
    #                    (from_dt != lag(from_dt)) | 
                        (!is.na(conf_num) & conf_num != last_resection)),
    1, 
    0
  )) %>%
  
  
  mutate(cum_resection = cumsum(unique_resc_indicator== 1)) 



## ---------------------- only selecting relevant data between 1st and 2nd resection -------------- ##

final_cohort <- cohort %>%
  
  filter(cum_resection > 0 & cum_resection <= 2)



###########################  introduce "visit" times in variables #####################################################
###### we want to create a distinct column of "visits".                                                               #
########## case 1: Hospitalization                                                                                    #
################## each hospitalization which has it's own conf_num code, will be a visit                             #
########## case 2: Outpatient visits                                                                                  #
################### We know when Dr's visits occurs because they will be accompanied by key words "office".           #
################### then, we will lump everything that happened (all claims) between dr apptment into a single visit. #
################### hospitalizations will be their own visits, and all associated claims will be part of that.        #


# introducing binary variables to show hospitalizations and confirmed outpatient drs visits
final_cohort <- final_cohort %>% 
  ### we index confirmed office visits as "outpatient events" based on filtering for the word "office" in the procedure description
  # refer to 03_identifying_scratch
  mutate(outpatient_event = ifelse( proc_cde == 99211 & is.na(conf_num)| proc_cde == 99212 & is.na(conf_num)| proc_cde == 99213 & is.na(conf_num)| 
                                      proc_cde == 99214 & is.na(conf_num)| proc_cde == 99215 & is.na(conf_num)| proc_cde == 99201 & is.na(conf_num)| 
                                      proc_cde == 99202 & is.na(conf_num)| proc_cde == 99203 & is.na(conf_num)| proc_cde == 99204 & is.na(conf_num)| 
                                      proc_cde == 99205 & is.na(conf_num)| proc_cde == 99241 & is.na(conf_num)| proc_cde == 99242 & is.na(conf_num)|
                                      proc_cde == 99243 & is.na(conf_num)| proc_cde == 99244 & is.na(conf_num)| proc_cde == 99245 & is.na(conf_num), 1, 0)) %>% 
  ## we index hospitalizations (have conf_num) as another event named "hospitalization event"
  mutate(hospitalization_event = ifelse(!is.na(conf_num), 1, 0))

## ----------------------- defining visits -----------------------##
## now we want to create a timeline of visit numbers between confirmed visits and hospitalizations for a patient by creating a "days since last event" column
## for each patient, we also want to create a "visit number" column to define the number of total visits they had
######## VARIABLES CREATED: ######
## 1. days_since_visit_start: 
#########   We index a visit by either a hospitalization or an outpatient "in office" visit. Claims can be filed days after the visit itself occurs,
#########      this variable counts the number of days since the visit occured between when the current claim is filed, since there can be lag
## 2. visit:
#########   The number of visits that a patient has had, including both outpatient and hospitalizations


  
final_cohort <- final_cohort %>% group_by(pat_id) %>% arrange(from_dt, desc(resection)) %>% #group by patient ID and arrange dates
  # within each patient, for each from_dt, we arrange so that the "office visit" will come first
  ungroup() %>% group_by(pat_id, from_dt) %>% arrange(desc(resection), desc(outpatient_event), desc(conf_num), .by_group = T) %>% ungroup() %>% group_by(pat_id) %>%
  
  #select(pat_id, claimno, from_dt, proc_cde, procedure_desc, 
  #conf_num, ndc, group, drug_name, drug_class, diagnosis, outpatient_event, hospitalization_event)  %>% 
  mutate(
    outpatient_event_logical = as.logical(outpatient_event),
    hospitalization_event_logical = as.logical(hospitalization_event),
    
    # converting non events (false) into "missing"values
    last_outpatient_event = if_else(outpatient_event_logical, true = from_dt, false = NA), 
    last_hospitalization_event = if_else(hospitalization_event_logical, true = from_dt, false = NA)) %>% 
  
  # then fill in the NAs with the last event (visit) date
  tidyr::fill(last_outpatient_event) %>% tidyr::fill(last_hospitalization_event, 
                                                     .direction = "down") %>% 
  
  # create a column for "last event" which compares the dates between the last confirmed outpatient event and hospitalization
  ## we want to choose the one that's more recent to be the last confirmed event
  
  mutate(last_event = pmax(last_outpatient_event, last_hospitalization_event, na.rm = TRUE)) %>% 
  
  # 1. create a days since first claim column subtracting the current from_dt from the date of the last confirmed visit.
  ### this shows that some claims are filed several days since the confirmed in office visits
  mutate(days_since_visit_start = from_dt - last_event) %>% mutate(year = lubridate::year(from_dt)) %>%
  
  ####### create column for the first instance of a conf_num since we get consecutive 1's for hospitalizations. If there is no conf_num, put a value of 0
  group_by(pat_id) %>%
  
  mutate(hosp_1_ins = ifelse(is.na(conf_num), 0, as.integer(!duplicated(conf_num) ))) %>% ungroup() %>%
  
  group_by(pat_id, from_dt) %>% 
  
  # Set `unique_outpatient_event` to 1 only for the first occurrence and 0 for the rest
  mutate(unique_outpatient_event = if_else(row_number() == 1 & any(outpatient_event == 1), 1, 0)) %>%
  
  
  # 2. create the number of visits that a patient has had.
  #### we increment the "visit" with every occurrence of a 1 (confirmed outpatient visit), and increment again with first instance of hospitalization
  #### this is done by using both the outpatient indicator and the hospitalization indicator
  rowwise() %>%
  mutate(
    # Exclude visits with both resection and conf_num
    cum_events = ifelse(resection == 1 & !is.na(conf_num), 0, sum(hosp_1_ins, unique_outpatient_event, na.rm = TRUE))) %>% 
  ungroup() %>% group_by(pat_id) %>% 
  mutate(visit = cumsum(cum_events== 1) ) %>% ungroup()



## ------------ calculating stage length ----------##
### if there is a next visit, stage length is current stage's first from_dt to next stage's first from_dt
## if there is no next visit, stage length is the current stage's first claim to last claim

final_cohort <- final_cohort %>%
  # Step 1: Group by patient and visit to get the earliest date (from_dt) for each visit
  group_by(pat_id, visit) %>%
  summarize(earliest_date = min(from_dt, na.rm = TRUE),
            latest_date = max(from_dt, na.rm = TRUE),
            .groups = 'drop') %>%
  ungroup() %>%
  
  # Step 2: Calculate the visit length by getting the earliest date of the next visit
  group_by(pat_id) %>%
  arrange(pat_id, visit) %>%
  mutate(
    earliest_nextvis_date = lead(earliest_date),  # Get the earliest date of the next visit
    ## if it's not the last visit, we use the first from_dt of the next visit, minus the from_dt from the current visit
    ## otherwise, if it's the max visit, we use the last from_dt of the current visit minus the first from_dt of the current visit
    visit.length = ifelse(visit != max(visit), earliest_nextvis_date - earliest_date + 1, latest_date - earliest_date + 1)  
  ) %>%
  ungroup() %>% left_join(final_cohort, by = c("pat_id", "visit"))



########################## Create a diagnosis column so we can see associated diagnoses of "Crohn's Disease" and "Ulcerative Colitis" ############

final_cohort$diagnosis <- ifelse(grepl("(\\b556)|(\\bK51)", final_cohort$diag1),"ulcerative_colitis",
                           ifelse(grepl("(\\b555)|(\\bK50)", final_cohort$diag1), "crohns", NA))


