
## Reading in data

library(readxl)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

#Read in excel file
ibs_dat <- read_excel("ibs_dat.xlsx", skip = 1)
