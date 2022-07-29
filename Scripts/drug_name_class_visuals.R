
library(lubridate)
library(dplyr)
library(ggplot2)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

##Add source file reading in data from the drug class categorization
source('Scripts/drug_class_categorization.R')


######### Visualizing the drug usage by name AND by class


# First wrangle data to get the visits dates, drug class, drugs used
# These are the distinct visit, drug class, and drug used-- only ones that have NDC codes
drug_usage <- ibs_drug_class %>%  distinct(from_dt, .keep_all = TRUE) %>% 
  arrange(ymd(.$from_dt)) %>% filter(ndc != "NA") %>%
  group_by(drug_class, drug_name, from_dt, claimno) %>%
  tally %>% mutate(month_day = format(from_dt, "%m-%d"),
                   year = format(from_dt, "%Y"), month = format(from_dt, "%m"))


# by drug name

ggplot(data = drug_usage, aes(x = from_dt, y = n, colour = drug_name)) +
  geom_bar(stat = "identity", aes(fill =  drug_name)) +
  labs(title ="Drug Name Prescribed per visit date",
       x = "Date", y = "Claims") + ylim(0, 3)


ggplot(data = drug_usage, aes(x = month_day, y = n, colour = drug_name)) +
  geom_bar(stat = "identity", aes(fill =  drug_name)) +
  labs(title ="Drug Name Prescribed per visit date by Year",
       x = "Date", y = "Claims") + ylim(0, 3) + facet_grid(facets = year ~ .) +
  scale_x_discrete("month_day", c("01-17", "02-17", "03-16", "04-17", "05-18", 
                                  "06-19", "07-17", "08-13", "09-12", "10-12", "11-12", "12-12"))


# by drug class
ggplot(data = drug_usage, aes(x = from_dt, y = n, colour = drug_class)) +
  geom_bar(stat = "identity", aes(fill =drug_class)) +
  labs(title ="Drug Class Prescribed per visit date",
       x = "Date", y = "Claims") + ylim(0, 3) + scale_fill_manual(values = c("orange", "blue", "gray")) + scale_colour_manual(values = c("orange", "blue", "gray"))


ggplot(data = drug_usage, aes(x = month_day, y = n, colour = drug_class)) +
  geom_bar(stat = "identity", aes(fill =  drug_class)) +
  labs(title ="Drug Class Prescribed per visit date by Year",
       x = "Date", y = "Claims") + ylim(0, 3) + scale_fill_manual(values = c("orange", "blue", "gray")) + scale_colour_manual(values = c("orange", "blue", "gray")) + facet_grid(facets = year ~ .) +
  scale_x_discrete("month_day", c("01-17", "02-17", "03-16", "04-17", "05-18", 
                                  "06-19", "07-17", "08-13", "09-12", "10-12", "11-12", "12-12"))


###### View list of drugs used in 2011--- not just ones that are interesting to us
###### for slide 12 time gap

## filter by 2011 only, then view drugs (NDC code exists) ordered by month, primarily between July (diagnosis) - December
###
ibs_dat %>% arrange(ymd(.$from_dt)) %>%
  mutate(year = format(from_dt, "%Y"), month = format(from_dt, "%m")) %>%
  filter(year == "2011" & ndc != "NA") %>%
  # tally by unique generic name for drugs
  group_by(from_dt, GENERIC_NAME) %>% tally


