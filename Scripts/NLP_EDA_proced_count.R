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


### NLP exploration

# library(tokenizers)
# library(tidytext)

# ## table of frequencies
# my_tab <- table(ibs_dat[,"PROCEDURE_DESC"]) 
# 
# ## order table & convert to DF
# ord_tab_df <- as.data.frame(my_tab[order(my_tab, decreasing = TRUE)])
# 
# write_xlsx(ord_tab_df, "/Users/janeshe/Desktop/RL_IBS/NLP_EDA_ordered_table.xlsx")


# ## Create vector just for procedure descriptions for ease of access & turn into phrases of 5 words
# desc <- tibble(txt = ibs_dat[,"PROCEDURE_DESC"])  %>% 
#   mutate_all(as.character) %>% 
#   unnest_tokens(word, txt, token="ngrams", n=5, to_lower = T) %>%
#   count(word) %>% 
#   arrange(desc(n))
#  

```
