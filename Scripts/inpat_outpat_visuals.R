
## Inpatient vs. Outpatient visualization
library(lubridate)
library(dplyr)


#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("~/RL_IBS")

##Add source file reading in data
source('Scripts/read_data.R')


######## Inpatient analysis #########

# already done by looking at hospitalizations

## pull out inpatient AKA hospitalizations (Have conf_num code) data
inpatient <- ibs_dat %>% filter(!is.na(conf_num))

# These are the distinct visit dates/distinct claims
inpatient_visits <- inpatient %>%  distinct(from_dt, .keep_all = TRUE) %>% 
  arrange(ymd(.$from_dt)) %>%
  group_by(from_dt, claimno) %>%
  tally


# graph of above for individual visit frequency
## Create plot with unique "from_dt" as x-axis, number of unique visits per date 

ggplot(data = inpatient_visits, aes(x = from_dt, y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title ="Unique Inpatient Visit Claims",
       x = "Date", y = "Claims") + ylim(0, 3)

### counting total claims- non uniqueness
inpatient_claim_counts <- inpatient %>% arrange(ymd(inpatient$from_dt)) %>%
  group_by(from_dt, claimno) %>%
  tally

## Create plot with unique "from_dt" as x-axis, number of TOTAL claims as y-axis
########## This graph isn't that helpful and needs to be broken down 
ggplot(data = inpatient_claim_counts, aes(x = from_dt, y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title ="Total Number of Outpatient Claims (non-unique) per visit date",
       x = "Date", y = "Number of (non-unique) Claims")


################################ Outpatient analysis ######################################

# get rid of pharmacy claims which have NDC codes

outpatient <- ibs_dat %>% filter(is.na(conf_num)) %>% filter(is.na(ndc))

## Visualize total outpatient claims
#### There are 220 total rows for this, but only 40 unique claims (distinct dates)

# These are the distinct visit dates/distinct claims
outpatient_visits <- outpatient %>%  distinct(from_dt, .keep_all = TRUE) %>% 
  arrange(ymd(.$from_dt)) %>%
  group_by(from_dt, claimno) %>%
  tally


# graph of above for individual visit frequency
## Create plot with unique "from_dt" as x-axis, number of unique visits per date 

ggplot(data = outpatient_visits, aes(x = from_dt, y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title ="Unique Outpatient Visit Claims",
       x = "Date", y = "Claims") + ylim(0, 5)


### counting total claims- non uniqueness
outpatient_claim_counts <- outpatient %>% arrange(ymd(outpatient$from_dt)) %>%
  group_by(from_dt, claimno) %>%
  tally

## Create plot with unique "from_dt" as x-axis, number of TOTAL claims as y-axis
########## This graph isn't that helpful and needs to be broken down 
ggplot(data = outpatient_claim_counts, aes(x = from_dt, y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title ="Total Number of Outpatient Claims (non-unique) per visit date",
       x = "Date", y = "Number of (non-unique) Claims")
