if(!('pacman' %in% installed.packages()[,"Package"])) install.packages("pacman")
library(pacman)
p_load(tidyverse)

load('data/akicreatinine.rda')
load('data/computed_labs.rda')
load('data/admissions.rda')
load('data/d_items.rda')
load('data/datetimeevents.rda')
load('data/kdigo_uo.rda')
load('data/kdigo_stages_48hr.rda')
load('data/norepinephrine_dose.rda')
load('data/ventfirstday.rda')

admissions <- admissions %>% 
  select(-one_of(c('ROW_ID', 'DEATHTIME', 'DISCHTIME', 
                   'DISCHARGE_LOCATION', 'EDREGTIME', 'EDOUTTIME', 'HOSPITAL_EXPIRE_FLAG',
                   'HAS_CHARTEVENTS_DATA')))

patients <- patients %>% 
  select(one_of(c('SUBJECT_ID', 'GENDER', 'DOB')))

# TODO: incorporate this info
# datetimeevents <- datetimeevents %>% 
#   select(one_of(c('SUBJECT_ID', 'HADM_ID', 'ITEMID', 'CHARTTIME', 'CGID'))) %>% 
#   left_join(d_items %>% select(one_of(c('ITEMID', 'LABEL')))) %>% 
#   left_join(akicreatinine %>% select(one_of('SUBJECT_ID', 'HADM_ID', 'INTIME'))) %>% 
#   mutate(INTIME_PLUS_24 = INTIME + (24 * 3600)) %>% 
#   mutate(BETWEEN_24 = ifelse((CHARTTIME >= INTIME & CHARTTIME <= INTIME_PLUS_24), 1, 0)) %>% 
#   filter(BETWEEN_24 == 1)

# TODO: :)
# FeNa <- (Sodium_Urine/Sodium)/(Creatinine_Urine/FIRST_CREATININE) * 100
# FeU <- ('Urea Nitrogen_Urine'/ US)/(Creatinine_Urine/FIRST_CREATININE) * 100

norepinephrine_dose <- norepinephrine_dose %>% 
  left_join(akicreatinine, by = c("icustay_id" = "ICUSTAY_ID")) %>% 
  filter((INTIME + (24 * 3600)) > endtime & INTIME <= starttime) %>% 
  select(one_of('icustay_id', 'vaso_rate', 'vaso_amount')) %>% 
  group_by(icustay_id) %>% 
  summarise(vaso_rate = mean(vaso_rate),
            vaso_amount = mean(vaso_amount))

ventfirstday <- ventfirstday %>%
  group_by(subject_id) %>% 
  summarise(aux_duplicate = max(vent)) %>% 
  ungroup()

akicreatinineplus <- akicreatinine %>%
  left_join(kdigo_stages_48hr %>% 
              select(one_of('icustay_id', 'AKI_48hr', 
                            'AKI_stage_48hr', 'AKI_stage_48hr_creat')), 
            by = c("ICUSTAY_ID" = "icustay_id")) %>% 
  left_join(kdigo_uo %>% 
              group_by(icustay_id) %>% 
              mutate(first_register = first(charttime)) %>% 
              filter(charttime == first_register) %>% 
              select(-one_of('charttime', 'first_register')), 
            by = c("ICUSTAY_ID" = "icustay_id")) %>%
  left_join(norepinephrine_dose,  by = c("ICUSTAY_ID" = "icustay_id")) %>% 
  left_join(ventfirstday,  by = c("SUBJECT_ID" = "subject_id")) %>% 
  left_join(admissions) %>% 
  left_join(patients) %>%
  mutate(YEAR_INTIME_DATE = year(as.Date(INTIME)),
         YEAR_DOB_DATE = year(as.Date(DOB))) %>% 
  mutate(EDAD = YEAR_INTIME_DATE - YEAR_DOB_DATE) %>% 
  mutate(EDAD = ifelse(EDAD > 90, 91, EDAD)) %>%
  select(-one_of('DOB', 'YEAR_INTIME_DATE', 'YEAR_DOB_DATE'))

save(akicreatinineplus, file = 'data/akicreatinineplus.rda')
