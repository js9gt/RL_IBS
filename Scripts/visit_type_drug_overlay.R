
## Plot overlay for visit type & drug class

library(dplyr)
library(ggplot2)
library(lubridate)


#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

##Add source file reading in data
source('Scripts/group_col_labs_imaging_etc.R')
source('Scripts/drug_name_class_visuals.R')


########## This graph isn't that helpful and needs to be broken down 
ggplot(data = group_claim_cts, aes(x = month_day, y = n)) +
  geom_bar(stat = "identity", aes(fill = group, colour = group)) +
  geom_point(data = drug_usage, stat = "identity", aes(fill =  drug_class, colour = drug_class), shape = 8, size = 3) + 
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue", "purple", "brown")) + scale_colour_manual(values = c("red", "orange", "yellow", "green", "blue", "purple", "brown")) +
  facet_grid(facets = year ~ .) +
  scale_x_discrete("month_day", c("01-18", "02-17", "03-16", "04-17", "05-18", 
                                  "06-19", "07-17", "08-13", "09-12", "10-12", "11-12", "12-12")) +
  labs(title ="Total Number of Claims (non-unique) per visit date",
       x = "Month", y = "Number of (non-unique) Claims")
