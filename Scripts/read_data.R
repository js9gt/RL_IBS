
## Reading in data

library(readxl)
library(stringr)

#Set WD to Project folder-- will want to change for future if running somewhere else
setwd("/Users/janeshe/Desktop/RL_IBS")

#Read in excel file
ibs_dat <- read_excel("ibs_dat.xlsx", skip = 1)


## Pad NDC codes to contain 0 in front so all are standardized length
#### COMEMNT---- turns NDC codes from numeric to a character when including 0's

ibs_dat$ndc <- str_pad(ibs_dat$ndc, max(nchar(na.omit(ibs_dat$ndc))), side="left", pad="0")
