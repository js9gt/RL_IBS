

## Reading in data

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("~/RL_IBS")

##Add source file reading in data
source('Scripts/03_censor_visit_grouping.R')

max_stages <- 3


### at each visit, we want to identify:
## the number of unique pts in the visit
## the % of unique "group" uses in the visit
## the % of unique "drug_class" uses in the visit

trt_categ <- final_cohort %>% select(pat_id, visit, group, drug_class) %>%
  # Remove duplicate group and drug_class entries within the same visit for the same patient
  distinct(pat_id, visit, group, drug_class, .keep_all = TRUE) %>%
  
  # Spread the group categories into separate columns with binary indicators
  tidyr::pivot_wider(names_from = group, values_from = group, 
              values_fn = length, values_fill = 0) %>%
  
  # Convert binary indicators for group categories
  mutate(across(starts_with("group_"), ~ ifelse(. > 0, 1, 0))) %>%
  
  ## remove the NA column, since when we pivot the drug_class column we will also have an NA
  select(-`NA`) %>%
  
  # Spread the drug_class categories into separate columns with binary indicators
  tidyr::pivot_wider(names_from = drug_class, values_from = drug_class, 
              values_fn = length, values_fill = 0) %>%
  
  # Convert binary indicators for drug_class categories
  mutate(across(starts_with("drug_class_"), ~ ifelse(. > 0, 1, 0))) %>%
  
  ## remove the NA column, since when we pivot the drug_class column we will also have an NA
  select(-`NA`) %>% 
  # Group by patient ID and sum up the binary indicators for each patient, ensuring no double counting
  group_by(pat_id, visit) %>%
  summarize(across(everything(), ~ pmin(sum(. > 0), 1))) %>%
  
  ungroup() %>% 
  
  # Summarize by visit to get the percentage use of each category
 group_by(visit) %>% 
  summarize(
    across(-pat_id, ~ mean(. > 0)*100),
    n = n_distinct(pat_id))  %>% 
  
  ungroup()


#writexl::write_xlsx(trt_categ, "../trt_categ.xlsx")


########
######## Preprocessing data for RDA
########

## ---- Variables we will use in our model ----- ##

# Baseline: age at first surgery (age_first_resc), sex (der_sex)
# Updated: # of times pt received trt (cumulative_anti_TNF, cumulative_no_anti_TNF), # past visits, cumulative time, state variable 
#########: state 1: # of previous hospitalizations (cumulative_hosp), state 2: # of days since last hosp (days_since_last_hosp_cum)



## 1. need to create a count of # times the treatment has been used in the past--
#### for us, we count any of the drug columns: 
## however, this count should not

final_cohort <- final_cohort %>%
  group_by(pat_id, visit) %>%
  
  # Create a binary indicator for whether "anti_TNF" was used in each visit
  mutate(A1.ind = ifelse(any(!is.na(drug_class)), 1, 0),
         A0.ind = 1 - A1.ind,
         A = ifelse(any(!is.na(drug_class)), 1, 0),) %>%
  
  # Ensure each visit has the same value for anti_TNF_used and no_anti_TNF within a visit
  ungroup() %>%
  
  # Select one row per visit for cumulative calculation
  group_by(pat_id, visit) %>% 
  slice(1) %>% # Select the first row per visit 
  ungroup() %>% group_by(pat_id) %>% 
  arrange(pat_id, visit) %>%
  
  # Calculate cumulative sums up to but not including the current visit
  mutate(A1.count = lag(cumsum(A1.ind), default = 0),
         A0.count = lag(cumsum(A0.ind), default = 0)) %>%
  
  
  select(pat_id, visit, visit.length, A1.count, A0.count, A) %>%
  
  # Join back to the original data to keep all rows per visit
  right_join(final_cohort, by = c("pat_id", "visit", "visit.length")) %>%
  
  # Fill the cumulative columns down for each visit
  group_by(pat_id, visit) %>%
  tidyr::fill(A1.count, A0.count, A, .direction = "downup") %>%
  
  ungroup() 


## 2. define 2 state variables as the proportion of days since the last hospitalization (denom is cumulative time) & number of previous hospitalizations
### days since last hospitalization is counted through the visit length
### numner of previous hospitalizations (doesn't count the current visit)

final_cohort <- final_cohort %>%
  group_by(pat_id, visit) %>%
  
  # Create a binary indicator for whether there was a hospitalization
  mutate(visit_hosp = ifelse( !is.na(conf_num), 1, 0)) %>% 
  
  ungroup() %>%
  
  # Select one row per visit for cumulative calculation
  group_by(pat_id, visit) %>% 
  slice(1) %>% # Select the first row per visit 
  ungroup() %>% group_by(pat_id) %>% 
  arrange(pat_id, visit) %>%
  
  # Calculate cumulative sums up to but not including the current visit
  mutate(cumulative.hosp = cumsum(visit_hosp), default = 0,
         visit_of_last_hosp = lag(if_else(visit_hosp == 1, visit, NA_integer_))) %>%
  
  
  select(pat_id, visit, conf_num, cumulative.hosp, visit_hosp, visit_of_last_hosp) %>% 
  
  # Join back to the original data to keep all rows per visit
  right_join(final_cohort, by = c("pat_id", "visit", "conf_num")) %>%
  
  # Adjust cumulative.hosp to be the minimum value within each visit
  group_by(pat_id, visit) %>%
  mutate(cumulative.hosp = min(cumulative.hosp, na.rm = TRUE)) %>% ungroup() %>%
  
  # Fill the cumulative columns down for each visit
  group_by(pat_id) %>%
  tidyr::fill(cumulative.hosp, visit_of_last_hosp, .direction = "down") %>%
  
 ungroup()

 

## 3. define baseline variables: age at first surgery--  year of first resection - der_YOB, sex (der_sex)

final_cohort <- final_cohort %>%
  group_by(pat_id) %>%
  
  # Find the earliest instance where unique_resc_indicator is 1 and calculate the age
  mutate(age.first.resc = ifelse(from_dt == min(from_dt[unique_resc_indicator == 1]), 
                                 year - der_yob, NA)) %>%
  # Fill the calculated age for all entries of the same patient
  # Fill the calculated age for all entries of the same paftient, ensuring we handle patients with no unique_resc_indicator == 1
  mutate(age.first.resc = ifelse(all(is.na(age.first.resc)), NA, first(na.omit(age.first.resc)))) %>%
  ## rename the patient sex column 
  rename(der.sex = der_sex)


##########################


## we only care about time from the first to the second hospitalization, which is our new outcome (or resection)
final_cohort <- final_cohort %>% filter(cumulative.hosp <= 2)


##########################

## 4. create column for number of visits previous, previous visit length
final_cohort <- final_cohort %>%
  mutate(
    nstageprev = ifelse(visit > 0, visit - 1, 0) ) %>%
  group_by(pat_id, visit) %>% slice(1) %>% ungroup() %>% 
  mutate(
    prev.visit.length = ifelse(visit > 1, lag(visit.length), 0)  # Shift previous visit length within each patient group
  ) %>% 
  select(pat_id, visit, nstageprev, prev.visit.length) %>%
  
  
  # Join back to the original data to keep all rows per visit
  right_join(final_cohort, by = c("pat_id", "visit")) %>%
  
  # Fill the cumulative columns down for each visit
  group_by(pat_id) %>%
  tidyr::fill(nstageprev, prev.visit.length, .direction = "down") %>%
  
  ungroup() 



## ------------ calculating cumulative time ----------##
## we want to create a cumulative_time column which consists of time elapsed since baseline
## for each visit, we will take the cumulative sum of all visit times (excluding baseline)

final_cohort <- final_cohort %>% group_by(pat_id, visit) %>% summarize(
  prev.visit.length = max(prev.visit.length, na.rm = TRUE),  # Ensure visit_length is the same for all entries in the same visit
  .groups = 'drop') %>% group_by(pat_id) %>%
  mutate(
    cumulative.time = cumsum(if_else(visit == 0, 0, prev.visit.length))   # Exclude visit 0 from cumulative sum
  ) %>% ungroup() %>% left_join(final_cohort, by = c("pat_id", "visit", "prev.visit.length")) %>%
  
  ungroup() %>%
  rename(subj.id = pat_id,
         stage = visit)

## --------------------- calculating censoring ------------------##
### only including the smallest visit number for the cumulative resection when there are 2, and include all the data for when there are less than 2
## for censoring (these are stage wise indicators): we only look at stages 1 +, since visit 0 is baseline
### not censored == 1
##### pt experiences event, pt experiences a next visit
### censored == 0
##### pt doesn't have event, or a next stage



## ------------- we can use cumulative.hosp instead of cum_resection, since cumulative.hosp is an earlier outcome

final_cohort <- final_cohort %>%
  group_by(subj.id, cumulative.hosp) %>% 
  filter(
    if (all(cumulative.hosp < 2)) {
      TRUE
    } else {
      stage == min(stage[cumulative.hosp == 2])
    }) %>% ungroup() %>%
  
  group_by(subj.id) %>%
  # Identify the maximum stage for each patient
  mutate(max_stage = max(stage, na.rm = TRUE)) %>%
  
  
  # Step 2: note that a patient is NOT censored if they get the event, and a patient is not censored if they have a next visit
  ## We will handle the issue with the elapsed time later
  ## we truncate at 25 visits
  
  # Step 3: Group by pat_id and visit, and create the stage-wise censoring indicator
  

#  group_by(subj.id, stage) %>%
#  mutate(delta = ifelse(cum_resection == 2, 1,
#                                                    ifelse(max_stage >= 20, 0,
#                                                           ifelse(stage < max_stage, 1, 0)))) %>%
#  ungroup() %>%
#  group_by(subj.id) %>%
#  ## if a patient for stages <= 25 ever has cum_resection == 2, they get a 1. Otherwise they get 0
#  mutate(overall.delta = ifelse(any(cum_resection == 2 & stage <= 20), 1, 0)) 


group_by(subj.id, stage) %>%
  mutate(delta = ifelse(
    ## if the patient experiences the second hospitalization, they are not censored (delta = 1)
    any(cumulative.hosp == 2), 
    1,
    ## otherwise, if we reach stage 20 (max number of stages), or we're reached a patient's final stage that's less than 20
    ## then the patient is censored
    ifelse(stage == max_stages | (stage == max_stage & max_stage < max_stages), 0, 1)
  )) %>%
  ungroup() %>%
  group_by(subj.id) %>%
  ## if any of the deltas were 0 from stages 1-20, the overall delta is 0 (censoring)
  ## otherwise, it's a 1
  #mutate(overall.delta = ifelse(any(cum_resection == 2 & stage <= 20), 1, 0)) %>%
  #ungroup()
  ## if the event occurred before stage 20 (2nd resection), then we have a value of 1. Otherwise, a value of 0 (censored)
  mutate(overall.delta = ifelse(any(cumulative.hosp == 2 & stage <= max_stages), 1, 0)) %>%
  ungroup()

#%>% 
#  select(subj.id, stage, visit.length, delta, overall.delta, cum_resection, cum.time) %>%
#  group_by(subj.id) %>% filter(stage <= 20) %>% arrange(desc(cum.time)) %>% slice(1) %>% View()



## ------------ calculating Tau ----------##

## we can look at the maximum of the cumulative time and plot its distribution by frequency

max_tau <- final_cohort %>% group_by(subj.id) %>% 
  ## first, we select only up to 25 visits
  filter(stage <= 25) %>%
  summarise(tau = max(cumulative.time, na.rm = T)) %>% ungroup()

# Step 2: Create a histogram of the maximum cumulative times
ggplot(max_tau, aes(x = tau)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Maximum Cumulative Times",
    x = "Maximum Cumulative Time",
    y = "Count"
  ) +
  theme_minimal()

## now, we will choose a tau of 2500, and make our cutoffs
## ---- A patient's last visit will then be the visit smaller than, but closest to the tau cutoff
## ---- we would also adjust their visit length to be until tau, and also mark the patient as censored for that stage

## we can compare this to when we calculate tau as a patient's last from_dt - their first from_dt, excluding visit == 0

tau <- 1000


final_cohort <- final_cohort %>%
  # Group by patient ID to handle each patient's visits separately
  group_by(subj.id) %>%
  
  ## we can only have 25 stages max
  filter(stage <= max_stages) %>%
  
  # Filter for stages where the cumulative time is less than or equal to 2500
  filter(cumulative.time <= tau) %>%
  
  # For each patient, select the row with the maximum cum.time (closest to 2500)
  slice_max(cumulative.time, with_ties = FALSE) %>%
  
  # Create a new column "truncatedvis" that calculates the difference between cum.time and the cutoff
  mutate(truncatedvis = tau - cumulative.time) %>%
  
  ungroup() %>% 
  # Apply the truncation logic
  mutate(
    ## if the visit length > truncated vis, then we should truncate the visit length
    ## also, we should censor the visit (delta == 0) due to being out of the study length
    visit.length_truncated = ifelse(visit.length > abs(truncatedvis), abs(truncatedvis), visit.length),  # Truncate visit.length if it's larger than truncatedvis
    delta_truncated = ifelse(visit.length == abs(truncatedvis), 0, delta)  # Update delta to 0 if truncation occurs
  ) %>% 
  
  select(subj.id, stage, visit.length_truncated, delta_truncated, truncatedvis) %>% 
  
  # Step 2: Join the processed data back into the original dataset
  right_join(final_cohort, by = c("subj.id", "stage")) %>%
  
  # Step 3: Apply truncation or retain original values
  mutate(
    visit.length = ifelse(!is.na(visit.length_truncated), visit.length_truncated, visit.length),
    delta = ifelse(!is.na(delta_truncated), delta_truncated, delta),
    truncatedvis = ifelse(is.na(truncatedvis), NA_real_, truncatedvis)  # Set truncatedvis to NA for non-truncated stages
  ) %>%
  
  # Step 4: Drop temporary columns
  select(-visit.length_truncated, -delta_truncated, -nstageprev, -prev.visit.length) %>%
  
  ungroup() 



## then, we need to re-adjust the previous visit length, and the cumulative sum

## 4. create column for number of visits previous, previous visit length
final_cohort <- final_cohort %>%
  mutate(
    nstageprev = ifelse(stage > 0, stage - 1, 0) ) %>%
  group_by(subj.id, stage) %>% slice(1) %>% ungroup() %>% 
  mutate(
    prev.visit.length = ifelse(stage > 1, lag(visit.length), 0)  # Shift previous visit length within each patient group
  ) %>% 
  select(subj.id, stage, nstageprev, prev.visit.length) %>%
  
  
  # Join back to the original data to keep all rows per visit
  right_join(final_cohort, by = c("subj.id", "stage")) %>%
  
  # Fill the cumulative columns down for each visit
  group_by(subj.id) %>%
  tidyr::fill(nstageprev, prev.visit.length, .direction = "down") %>%
  
  ungroup() %>% select(-cumulative.time)


## ------------ calculating cumulative time ----------##
## we want to create a cumulative_time column which consists of time elapsed since baseline
## for each visit, we will take the cumulative sum of all visit times (excluding baseline)

final_cohort <- final_cohort %>% group_by(subj.id, stage) %>% summarize(
  prev.visit.length = max(prev.visit.length, na.rm = TRUE),  # Ensure visit_length is the same for all entries in the same visit
  .groups = 'drop') %>% group_by(subj.id) %>%
  mutate(
    cumulative.time = cumsum(if_else(stage == 0, 0, prev.visit.length))   # Exclude visit 0 from cumulative sum
  ) %>% ungroup() %>% left_join(final_cohort, by = c("subj.id", "stage", "prev.visit.length")) %>%
  
  ungroup() 


# Step 1: Calculate delta and overall.delta based on unique subj.id and stage

## ------------------- we use cumulative.hosp instead of cum_resection since that happens sooner ----------------- ##

delta_summary <- final_cohort %>%
  group_by(subj.id, stage) %>%
  arrange(desc(cumulative.hosp)) %>% 
  slice(1) %>%
  ungroup() %>%
  group_by(subj.id) %>%
  mutate(
    delta = if_else(!is.na(lead(cumulative.time)) & lead(cumulative.time) == tau, 0, delta)  # Retain original delta if lead(cum.time) is NA
  ) %>%
  ungroup() %>% 
  group_by(subj.id) %>%
  #mutate(overall.delta = ifelse(any(delta == 0 & stage <= 20), 0, 1)) %>%
  mutate(overall.delta = ifelse(any(cumulative.hosp >= 2 & stage <= max_stages), 1, 0)) %>%
  select(subj.id, stage, delta, overall.delta) %>%  # Keep only necessary columns for join
  ungroup()

# Step 2: Join the summarized delta and overall.delta back to the original dataset
final_cohort <- final_cohort %>% select(-delta, -overall.delta) %>%
  left_join(delta_summary, by = c("subj.id", "stage"))

########


#### turn A into factor
final_cohort$A <- as.factor(final_cohort$A)
#### turn sex into factor
final_cohort$der.sex <- as.factor(final_cohort$der.sex)
final_cohort$subj.id <- as.factor(final_cohort$subj.id)




## ------------- pivoting the data to wide form -----------##
analysis_dat <- final_cohort %>% select(subj.id, stage, visit.length, prev.visit.length,
                                        cumulative.time, nstageprev, A, A1.count,
                                        A0.count, 
                                        #days.since.last.hosp.cum, 
                                        #cumulative.hosp, 
                                        der.sex, age.first.resc, delta, overall.delta) %>% 
  
  ## Convert cumulative time to a proportion of tau
  mutate(cumulative.time = cumulative.time / tau) %>%
  
  ## ignoring the visit at which patients received surgery
  ## we truncate at 25 stages
  filter(stage >= 1 & stage <= max_stages & cumulative.time <= 1) %>% 
  ## now, we slice just one visit, since all of the variables at each visit should be the same
  group_by(subj.id, stage) %>% slice(1) %>% # Pivot data to have separate columns for each visit, prefixing column names with visit number
  ungroup() %>% group_by(subj.id) %>% mutate(overall.time = sum(visit.length)) %>% 
  ## exclude the subject with missing birth year:
  
  filter(!subj.id == "p6u2merrzgegbs72") %>% 
  tidyr::pivot_wider(
    names_from = stage,       # Use the 'visit' column to create new columns
    values_from = c(visit.length, prev.visit.length,
                    cumulative.time, nstageprev, A, A1.count,
                    A0.count, 
                    #days.since.last.hosp.cum, 
                    #cumulative.hosp, 
                    der.sex, age.first.resc, delta),  # Specify which columns to reshape
    names_glue = "{.value}_{stage}"  # Dynamically name columns by combining column name and visit number
  ) %>% as.data.frame()



