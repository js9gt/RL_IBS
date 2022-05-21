
##Add source file reading in data
source('Scripts/read_data.R')

### Dates for events of interest (IBS, ED, surgeries, imaging)

## IBS diagnosis 
ibs_dat %>% filter(Primary_diag == "Irritable Bowel Syndrome")

### ED visit info by searching the PROCEDURE_DESC for instances of "emergency department"
ibs_dat %>% filter(grepl("emergency department", PROCEDURE_DESC, ignore.case = TRUE)) %>%
  group_by(from_dt)

### colonoscopy visits: replace with "colonoscopy"
### Esophagogastroduodenoscopy visits: replace with "Esophagogastroduodenoscopy"


### Some surgery info ... filter by (Enterectomy, Closure Of Enterostomy)
ibs_dat %>% filter(grepl("Enterectomy", PROCEDURE_DESC, ignore.case = TRUE) | 
                     grepl("Enterostomy", PROCEDURE_DESC, ignore.case = TRUE)) %>%
  group_by(from_dt)

### imaging visits: replace with "ultrasound, tomography"

