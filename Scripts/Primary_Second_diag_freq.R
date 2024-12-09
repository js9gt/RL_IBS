
## Get frequency count of primary and secondary diagnoses

## specific events of interest to look into (I can pull out dates & also add them to our visualizations)
## can pull out relative IBS diagnosis date-- claim date probs close to actual day of diagnosis

### NLP EDA-- procedure count
library(writexl)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("~/RL_IBS")

##Add source file reading in data
source('Scripts/read_data.R')

## table of frequencies for primary and secondary diagnosis
my_tab <- table(ibs_dat[,"Primary_diag"]) 
my_tab2 <- table(ibs_dat[,"Secondary_diag"], useNA = "always") 

## order table & convert to DF
ord_tab_df <- as.data.frame(my_tab[order(my_tab, decreasing = TRUE)])
ord_tab_df2 <- as.data.frame(my_tab2[order(my_tab2, decreasing = TRUE)])

## generating output table
write_xlsx(ord_tab_df, "/Users/janeshe/Desktop/RL_IBS/Outputs/PRIM_DIAG_ordered_table.xlsx")
write_xlsx(ord_tab_df2, "/Users/janeshe/Desktop/RL_IBS/Outputs/SECOND_DIAG_ordered_table.xlsx")
