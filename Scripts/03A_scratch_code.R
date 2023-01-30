

### Subset our outpatient visits through the new column "confirmed_visit"
outpatient <- cohort %>% filter(visit_event == 1)

## make a column that contains the length of time that's passed between the next from_dt and the current from dt

outpatient <- outpatient %>% 
  # turn from_dt into a date
  mutate(date = lubridate::ymd(from_dt)) %>%
  # group by patient ID
  group_by(pat_id) %>%
  arrange(from_dt) %>%
  #Calculate number of days between visits
  mutate(between_dt = as.numeric(date - lag(date)))
