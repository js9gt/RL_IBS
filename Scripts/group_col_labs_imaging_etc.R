library(dplyr)
library(ggplot2)
library(lubridate)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

##Add source file reading in data
source('Scripts/read_cohort_data.R')

#### Create plot with unique "from_dt" as x-axis, number of TOTAL claims as y-axis, color by the group
## also facet by year 

### unique claims & counts per date
group_claim_cts <- cohort %>% arrange(ymd(cohort$from_dt)) %>%
  group_by(from_dt, claimno, group) %>%
  tally %>% mutate(month_day = format(from_dt, "%m-%d"),
                   year = format(from_dt, "%Y"), month = format(from_dt, "%m"))

########## This graph isn't that helpful and needs to be broken down 
ggplot(data = group_claim_cts, aes(x = month_day, y = n, colour = group)) +
  geom_bar(stat = "identity", aes(fill = group)) +
  labs(title ="Total Number of Claims (non-unique) per visit date",
       x = "Month", y = "Number of (non-unique) Claims") + facet_grid(facets = year ~ .) +
  scale_x_discrete("month_day", c("01-18", "02-17", "03-16", "04-17", "05-18", 
                                  "06-19", "07-17", "08-13", "09-12", "10-12", "11-12", "12-12"))
