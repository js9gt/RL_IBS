### NLP EDA-- procedure count
library(writexl)

##Add source file reading in data
source('Scripts/read_data.R')

## table of frequencies
my_tab <- table(ibs_dat[,"PROCEDURE_DESC"]) 

## order table & convert to DF
ord_tab_df <- as.data.frame(my_tab[order(my_tab, decreasing = TRUE)])

## generating output table
write_xlsx(ord_tab_df, "/Users/janeshe/Desktop/RL_IBS/Outputs/NLP_EDA_ordered_table.xlsx")
