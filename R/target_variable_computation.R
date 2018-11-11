if(!('pacman' %in% installed.packages()[,"Package"])) install.packages("pacman")
library(pacman)
p_load(tidyverse, bigrquery)

load('data/labevents.rda')
load('data/icustays.rda')
  
db_creatinine <- labevents %>% 
  filter(ITEMID == 50912) %>% 
  filter(!is.na(HADM_ID)) %>%
  left_join(icustays, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  group_by(SUBJECT_ID) %>% 
  mutate(FIRST_ADMISSION = min(CHARTTIME)) %>% 
  ungroup() %>% 
  select(one_of(c('SUBJECT_ID', 'HADM_ID', 'ICUSTAY_ID', 'CHARTTIME', 'VALUENUM', 'INTIME')))

hadm_id_first <- db_creatinine %>% 
  filter(CHARTTIME > INTIME)

db_creatine_final <- db_creatinine %>% 
  filter(HADM_ID %in% hadm_id_first$HADM_ID) %>% 
  arrange(HADM_ID, CHARTTIME) %>% 
  mutate(TIME_DIFF = c(difftime(tail(CHARTTIME, -1), head(CHARTTIME, -1)),0) / 3600,
         TIME_DIFF_R_FIRST_TIME_ADMISSION_UCI = (CHARTTIME - INTIME) / 3600) %>% 
  mutate(DIFF_TO_24 = TIME_DIFF_R_FIRST_TIME_ADMISSION_UCI - 24,
         DIFF_TO_48 = TIME_DIFF_R_FIRST_TIME_ADMISSION_UCI - 48) %>% 
  group_by(HADM_ID) %>% 
  mutate(MIN_TO_24 = min(abs(DIFF_TO_24)),
         MIN_TO_48 = min(abs(DIFF_TO_48))) %>% 
  filter(TIME_DIFF_R_FIRST_TIME_ADMISSION_UCI >= 0) %>%
  ungroup() %>%
  arrange(HADM_ID, CHARTTIME) %>%
  group_by(HADM_ID) %>% 
  mutate(FIRST_CREATININE = first(VALUENUM)) %>% 
  filter(MIN_TO_48 == abs(DIFF_TO_48))

akicreatinine <- db_creatine_final %>% 
  select(one_of(c('SUBJECT_ID', 'HADM_ID', 'ICUSTAY_ID', 'INTIME', 'FIRST_CREATININE', 'VALUENUM'))) %>% 
  mutate(RATIO_C48_C0 = VALUENUM / FIRST_CREATININE) %>% 
  mutate(AKI_CREATININA = ifelse(RATIO_C48_C0 < 1.5, 0, 1)) %>% 
  group_by(SUBJECT_ID) %>% 
  mutate(FIRST_ADMISSION_UCI = min(INTIME)) %>% 
  filter(INTIME == FIRST_ADMISSION_UCI) %>% 
  drop_na() %>% 
  rename(CREATININE_48 = VALUENUM) %>% 
  select(one_of(c('SUBJECT_ID', 'HADM_ID', 'ICUSTAY_ID', 'INTIME', 'FIRST_CREATININE', 'CREATININE_48', 'RATIO_C48_C0', 'AKI_CREATININA')))

save(akicreatinine, file = 'data/akicreatinine.rda')
