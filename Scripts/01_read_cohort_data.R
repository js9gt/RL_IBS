
setwd("~/RL_IBS/Data")

library(dplyr)
library(readxl)
library(stringr)
library(haven)
library(withr)
library(ggplot2)
library(data.table)

cohort <- read_sas("cohort_claim.sas7bdat", NULL)

procedure_description <- read.csv("Pp_pr_lookup.csv")

patient_diagnoses <- read.csv("pp_dx_lookup.csv")

enroll <- read_sas("cohort_enroll.sas7bdat", NULL) %>% select(pat_id,der_sex, der_yob)

### combine the procedure descriptions 

# first remove special character from the procedure code
procedure_description$procedure_cd <- gsub('[^[:alnum:] ]','',procedure_description$procedure_cd)

# then subset just the procedure code and procedure description to bind to the cohort data based on matching codes
procedure_description <- procedure_description %>% select(procedure_cd, procedure_desc) %>% 
  # renaming 
  rename("proc_cde" = "procedure_cd")

# merging the descriptions and cohort data by the procedure code, keeping all the observations in "cohort" even when nothing matches up
cohort <- merge(procedure_description, cohort, by = "proc_cde", all.y = TRUE) 

# for the "enroll" dataset, we only care about the patient gender and YOB, so we just subset those to be merged by the patient ID
cohort <- merge(enroll, cohort, by = "pat_id", all.y = TRUE) 

