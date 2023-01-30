

setwd("/Users/janeshe/Desktop/RL_IBS")

source('Scripts/03_identifying_outpatient_visits.R')


# we have a binary column called "visit_event" indexing 1, if patient had a CONFIRMED visit, 0 otherwise

## TASK: We have confirmed visits, but between those, we still see instances of claims being filed on different days
# we want to calculate the number of days between claims and the last confirmed visit
# count these up over all patients so we can decide how to bundle


outpatient <- outpatient %>% group_by(pat_id) %>% arrange(from_dt) %>% #group by patient ID and arrange dates
  mutate(
    visit_event_logical = as.logical(visit_event),
    # converting non events (false) into "missing"values
    last_event = if_else(visit_event_logical, true = from_dt, false = NA_real_)) %>%
  # then fill in the NAs with the last event (visit) date
  tidyr::fill(last_event) %>%
  # create a days since last visit column subtracting the current from_dt from the date of the last confirmed visit
  mutate(days_since_visit = from_dt - last_event)

## for meeting viewing procedure descriptions-- export as excel
##################################### 

library("writexl")
write_xlsx(a,"outpatient_proc_descriptions.xlsx")


a <- outpatient %>% ungroup() %>% filter(visit_event == 1) %>% count(proc_cde, procedure_desc) %>% arrange(n)

#####################################


######## use frequency curve to visualize and find cutoffs ###########
num_freq <- outpatient %>% select(pat_id, from_dt, days_since_visit)%>% distinct() %>%
  ## the same claim can show multiple times, so we need to select unique patient, from_dt, between_dt
  ## distinct() selects unique combinations of these
  group_by(days_since_visit) %>% count() %>% na.omit()
names(num_freq) <- c("days_since_last_visit", "count")
# convert column into numeric for plotting
num_freq$days_since_last_visit <- as.numeric(num_freq$days_since_last_visit)

##################################### filter patients with days between visits == 7 #####################################
# include only variables of interest

dat_35_days <- outpatient %>% filter(days_since_visit == 35) %>% select(pat_id, from_dt, proc_cde, procedure_desc, 
                                                        diag1, diag2, group, diagnosis, last_event,
                                                        days_since_visit)
write_xlsx(dat_35_days,"35_days_between_data.xlsx")


## count procedure codes & procedure descriptions, count groups, count diagnosis


########################################################################################################################


## bar plot for frequency
# note: limiting x range
ggplot(data= num_freq, aes(x=days_since_last_visit, y=count)) +
  geom_bar(stat="identity") + xlim(0,100) + ylim(0, 3000)


##### looking at patients w/ really long days between visits

outpatient %>% select(pat_id, from_dt, procedure_desc, days_since_visit)

################# how do we group visits? #####################



## group time period with more than 3 days (aka 4 or more days are grouped as separate visits--)
## #increment the group value every time the difference is greater than 3 days, if it's greater than 
outpatient <- outpatient %>% group_by(pat_id) %>% mutate(visit_group = cumsum(ifelse(difftime(from_dt, shift(from_dt, fill = from_dt[1]), 
                                                                                              units = "days") > 3, 1, 0)) + 1)



### To view "accuracy" of visit divisions
#outpatient %>% select(from_dt, visit_group) %>% View()





# Calculating
subs <- outpatient %>% distinct(from_dt, visit_group, between_dt) %>% select(from_dt, visit_group, between_dt)

#Our distinct patient ID with more than 3 days between visits, then take tally of the total number of these entries
tallies <- subs %>% group_by(pat_id) %>% filter(between_dt > 3) %>% distinct(pat_id, from_dt, .keep_all = TRUE) %>% dplyr::tally()

#Total number of unique groups (distinct visits should be their own group)

total <- subs %>% group_by(pat_id) %>% distinct(pat_id, visit_group) %>% dplyr::tally() 

#Merge tallies w/ total
merged <- merge(tallies,total,by="pat_id")
names(merged) <- c("pat_id", "more_3", "total")

#we add 1 because the "lag" days have 1 fewer row as they calculate time in between, so the first row for the first group will never "count" in the numerator
robustness <- transform(merged, new = (more_3 + 1) / total)

#The way that we split this grouping up by 3 day windows matches up 100% in both methods (just as confirmation)



######## Now we want to look at frequency 

frequency <- outpatient %>% group_by(pat_id) %>% arrange(from_dt) %>% select(pat_id, from_dt, visit_group, between_dt) %>% 
  filter(!is.na(between_dt)) %>%
  distinct(from_dt,visit_group, .keep_all = TRUE) %>% group_by(pat_id) %>% 
  #Create a new column for frequency of distinct time between visits for each patient and create proportion to see quartile
  count(between_dt)


ggplot(frequency, aes(x=between_dt)) + geom_histogram(binwidth=1) + xlim(0, 8) #xlim(0, 100) for overall 

range(frequency$between_dt)

#### Quartile code
# %>% mutate(quartile = cumsum(n / sum(n))) %>% 
  #subset only the ones between the 45-55th quartile
  # filter(quartile > 0.45 & quartile < 0.55) %>% mean(between_dt) %>% View()



