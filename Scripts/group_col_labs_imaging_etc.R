library(dplyr)
library(ggplot2)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

##Add source file reading in data
source('Scripts/read_data.R')


## Create new column called "group"-- these will have the option to be:
# Prescriptions
# Imaging
# Labs
# Large Bowel Description
# Small Bowel Description
# Ileo-cecal Description
# Colonoscopy

ibs_dat$group <- 0

## we know prescription claims have NDC codes-- for anything with an NDC code, rewrite that as the "prescription" group
ibs_grouped <- ibs_dat %>%
  
  ## group the labs
  mutate(group = case_when(
    ## CBC w/ diff
    proc_cde ==  "85025" |
      ## C reactive protein
      proc_cde == "86140" | 
      ## Erythrocyte sedimentation rate
      proc_cde == "85651" | 
      proc_cde == "85652" |
      ## Albumin
      proc_cde == "82040" |
      ## AST
      proc_cde == "84450" |
      ## ALT
      proc_cde == "84460" |
      ## Total protein
      proc_cde == "84155"
    ~ "labs", 
    
    ## group imaging   
    ## CT abdomen w/ & w/o contrast
    proc_cde == "74170" | 
      ## CT absomen & pelvis w/o contrast
      proc_cde == "74176" | 
      ## CT absomen & pelvis w/ contrast 
      proc_cde == "74177" |
      ## MRI abdomen
      proc_cde == "74183" |
      ## MR enterography
      proc_cde == "74181"|
      proc_cde == "74182" |
      proc_cde == "74183"|
      proc_cde == "72195" |
      proc_cde == "72196"|
      proc_cde == "72197" |
      ## Xray abdomen
      proc_cde == "74018"
    ~ "imaging",
    
    ## group prescriptions (anything that has a non-NA NDC code)
    ndc != "NA" ~ "prescriptions",
    
    ## group Large Bowel Description
    proc_cde ==  "44140" |
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
      proc_cde == "44212"    
    ~ "large_bowel",
    
    ## group Small Bowel Description
    proc_cde ==  "44120" |
      proc_cde == "44121"| 
      proc_cde == "44125" | 
      proc_cde == "44202"    ~ "small_bowel",
    
    ## group ILEO-CECAL Description
    proc_cde ==  "44160" |
      proc_cde == "44205" 
    ~ "ileocecal",
    ## group colonoscopy
    proc_cde ==  "45378" |
      proc_cde == "45380" 
    ~ "colonoscopy"))


#### Create plot with unique "from_dt" as x-axis, number of TOTAL claims as y-axis, color by the group
## also facet by year 

### unique claims & counts per date
group_claim_cts <- ibs_grouped %>% arrange(ymd(ibs_dat$from_dt)) %>%
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
