if(!('pacman' %in% installed.packages()[,"Package"])) install.packages("pacman")
library(pacman)
p_load(tidyverse, recipes, progress, h2o, lime, mosaic)
source('functions.R')

h2o::h2o.init()
load('data/validatedbyexpertsdataset.rda')

datatomodel <- akicreatinine_full %>%
  filter(FIRST_CREATININE < 3 | RATIO_C48_C0 > 1.5) %>%
  select(-one_of(c('HADM_ID', 'ICUSTAY_ID', 'INTIME', 
                   'CREATININE_48', 'RATIO_C48_C0', 
                   'AKI_48hr', 'AKI_stage_48hr',
                   'AKI_stage_48hr_creat', 'ADMITTIME',
                   'aux_duplicate', 'DIAGNOSIS')))

for(column_name in colnames(datatomodel)) {
  if(is.character(datatomodel[, column_name]))
    datatomodel[, column_name] <- as.factor(datatomodel[, column_name])
}

inspect_datatomodel <- inspect(datatomodel)

datatoremove <- inspect_datatomodel$quantitative %>% 
  filter(n == 0)

datatomodelfinal <- datatomodel %>% 
  select(-one_of(datatoremove$name))

exploration_models <- modeling_exploration(datatomodelfinal, 'AKI_CREATININA')
best_models <- evaluate_explored_models(exploration_models$models)

train_lime <- as.data.frame(exploration_models$dataused$train)
explainer_lime <- lime::lime(train_lime, model = exploration_models$models$gbm[[1]]$model)

validation_lime <- as.data.frame(exploration_models$dataused$validation)
validation_lime_dr <-  validation_lime %>% 
  filter(AKI_CREATININA == 1)
validation_lime_dr <- validation_lime_dr[sample(nrow(validation_lime_dr), 3), ]

validation_lime_ndr <-  validation_lime %>% 
  filter(AKI_CREATININA == 0)
validation_lime_ndr <- validation_lime_ndr[sample(nrow(validation_lime_ndr), 3), ]

explanation_lime_dr <- lime::explain(validation_lime_dr, explainer_lime, n_labels = 2, n_features = 5)
explanation_lime_ndr <- lime::explain(validation_lime_ndr, explainer_lime, n_labels = 2, n_features = 5)

lime::plot_features(explanation_lime_dr)
lime::plot_features(explanation_lime_ndr)

h2o::h2o.shutdown(FALSE)
