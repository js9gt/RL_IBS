
## Get hospitalization lengths of stay

##Add source file reading in data
source('Scripts/read_data.R')

# first subset to the hospitalizations, then group by hospitalization code
# then order by from_dt and tally to see the number of claims on each unique date
# find difference between the first claim date and last claim date + 1 for each unique hospitalization to get total length

# only run before ### to get table w/ unique lengths
# for better graphics, need to mutate into year & month to capture time frame

hosp_duration <- ibs_dat %>% filter(!is.na(conf_num)) %>% 
  group_by(conf_num, from_dt) %>% arrange(from_dt) %>% tally() %>% #only include the distinct() line for small table
  mutate(hosp_length = max(ymd(from_dt)) - min(ymd(from_dt)) + 1) %>% # distinct(conf_num, hosp_length) ####
  distinct(from_dt, .keep_all = TRUE) %>% mutate(month = format(from_dt, "%m"),
                                               year = format(from_dt, "%Y"))


ggplot(data = hosp_duration[hosp_duration$year == 2011, ], 
       aes(x = as.numeric(month), y = as.numeric(hosp_length), fill = conf_num)) + 
  geom_bar(stat='identity') + scale_x_continuous(n.breaks = 6) + 
  labs(title ="Hospitalizations by Month in 2011",
       x = "Month", y = "Hospitalization Length (Days)")


# double checkina against what we had earlier
# now shows that initially just grouping by distinct dates (below) was the wrong approach since there are time gaps

ibs_dat %>% filter(conf_num == "jd8kqo0t2bd1r4dn") %>% group_by(from_dt) %>% distinct(from_dt)

## For others, replace with "ojunniviggd1r4dn" (4 days in Dec)
# "rxfbfog2rdd1r4dn" 15 days in 2011
