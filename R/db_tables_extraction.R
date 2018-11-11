if(!('pacman' %in% installed.packages()[,"Package"])) install.packages("pacman")
library(pacman)
p_load(tidyverse, bigrquery)
load(file = 'data/sepsis_detection.rda')

project_id <- "datathon-tarragona-2018"
options(httr_oauth_cache = FALSE)

# Wrapper for running BigQuery queries.
run_query <- function(query){
  data <- query_exec(query, project = project_id, use_legacy_sql = FALSE, max_pages = Inf)
  return(data)
}

take_data_derived <- function(table_name, int = FALSE) {
    alias <-  'mimiciii_derived.'
    query = paste0("SELECT * FROM `physionet-data.",
                   alias,
                   table_name,
                   "`")
  return(query)
}

take_n_items_derived <- function(sepsis_patients, table_name, clinical = TRUE, int = FALSE) {
    alias <- 'mimiciii_derived.'
    query = paste0("SELECT count(*) FROM `physionet-data.",
                   alias,
                   table_name,
                   "`")
  return(query)
}

tables_derived <- c('elixhauser_quan','norepinephrine_dose','qsofa','sapsii','sirs','ventfirstday',
                    'vitalsfirstday','weightfirstday','uofirstday','kdigo_creat','kdigo_stages_48hr','kdigo_uo')

n_items_derived <- NULL
for (table_name in tables_derived) {
  n_items_derived <- c(n_items_derived, unlist(run_query(take_n_items_derived(sepsis_patients, table_name))))
}
n_rows_tables_derived <- data.frame(table_name = tables_derived, n_rows = n_items_derived) %>% 
  arrange(by = n_rows)
save(n_rows_tables_derived, file = 'data/n_rows_tables_derived.rda')

for (table_name in n_rows_tables_derived$table_name) {
  query_sintax <- take_data_derived(table_name, TRUE)
  assign(table_name, run_query(query_sintax))
  save(list = table_name,
       file = paste0('data/', table_name, '.rda'))
  rm(list = table_name)
}

sepsis_patients <- sepsis_detection %>% 
  filter(angus == 1)

# Take data from hadm_id
take_data_filtered_clinical <- function(sepsis_patients, table_name) {
  subject_id <- sepsis_patients$subject_id %>% 
    unique() %>% paste0(collapse = ", ")
    alias <- 'mimiciii_clinical.'
  query = paste0("SELECT * FROM `physionet-data.",
                 alias,
                 table_name,
                 "` WHERE hadm_id IN (",
                 subject_id, ")")
  return(query)
}

take_n_items_clinical <- function(sepsis_patients, table_name) {
  hadm_ids <- sepsis_patients$hadm_id %>% 
    unique() %>% paste0(collapse = ", ")
  alias <- 'mimiciii_clinical.'
  query = paste0("SELECT count(hadm_id) FROM `physionet-data.",
                 alias,
                 table_name,
                 "` WHERE hadm_id IN (",
                 hadm_ids, ")")
  return(query)
}

dictionaries <- c('d_cpt', 'd_icd_diagnoses', 'd_icd_procedures',
                  'd_items', 'd_labitems')

for (dictionary in dictionaries) {
  assign(dictionary, run_query(paste0("SELECT * FROM `physionet-data.mimiciii_clinical.", dictionary, "`")))
  save(list = dictionary,
       file = paste0('data/', dictionary, '.rda'))
}

tables_clinical <- c('admissions', 'chartevents', 'cptevents',
                     'datetimeevents', 'diagnoses_icd', 'icustays',
                     'inputevents_cv', 'inputevents_mv', 'labevents',
                     'microbiologyevents', 'outputevents', 'patients',
                     'prescriptions', 'procedureevents_mv', 'procedures_icd',
                     'transfers')

n_items_clinical <- NULL
for (table_name in tables_clinical) {
  n_items_clinical <- c(n_items_clinical, unlist(run_query(take_n_items(sepsis_patients, table_name))))
}
n_rows_tables_clinical <- data.frame(table_name = tables_clinical, n_rows = n_items_clinical) %>% 
  arrange(by = n_rows)
save(n_rows_tables_clinical, file = 'data/n_rows_tables_clinical.rda')

for (table_name in n_rows_tables_clinical$table_name) {
  query_sintax <- take_data_filtered(sepsis_patients, table_name)
  assign(table_name, run_query(query_sintax))
  save(list = table_name,
       file = paste0('data/', table_name, '.rda'))
  rm(list = table_name)
}


