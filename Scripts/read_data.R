
## Reading in data

library(readxl)
library(stringr)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

#Read in excel file
ibs_dat <- read_excel("ibs_dat.xlsx", skip = 1)


#Read in excel file for 4 additional patients
dat_4pts <- read_excel("data_4pts.xlsx", 
                        col_types = c("text", "numeric", "text", 
                                       "text", "text", "text", "text", "numeric", 
                                       "text", "numeric", "numeric", "text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "text", "text", 
                                       "text", "text", "numeric", "text", 
                                       "numeric", "numeric", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "numeric", "numeric", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "numeric", "text", 
                                       "numeric"))

## Pad NDC codes to contain 0 in front so all are standardized length
#### COMEMNT---- turns NDC codes from numeric to a character when including 0's

ibs_dat$ndc <- str_pad(ibs_dat$ndc, max(nchar(na.omit(ibs_dat$ndc))), side="left", pad="0")

## do the same for 4 pts NDC codes

dat_4pts$ndc <- str_pad(dat_4pts$ndc, max(nchar(na.omit(dat_4pts$ndc))), side="left", pad="0")




