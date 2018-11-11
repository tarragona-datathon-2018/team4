modeling_exploration <- function(design_matrix_feature_engineering, y) {
  x <- setdiff(names(design_matrix_feature_engineering), c(y))
  
  hold_out <- design_matrix_feature_engineering
  hold_out <- h2o::as.h2o(hold_out)
  
  gbm_params <- list(
    learn_rate = seq(0.01, 0.1, 0.01),
    max_depth = seq(2, 10, 1),
    sample_rate = seq(0.5, 1.0, 0.1),
    col_sample_rate = seq(0.1, 1.0, 0.1)
  )
  
  search_criteria <- list(strategy = "RandomDiscrete", max_models = 5, seed = 15917)
  
  activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
  l1_opt <- c(0.00001, 0.0001, 0.001, 0.01, 0.1)
  l2_opt <- c(0.00001, 0.0001, 0.001, 0.01, 0.1)
  dl_params <- list(
    activation = activation_opt,
    l1 = l1_opt,
    l2 = l2_opt
  )
  count_aux <- 0
  glm_models <- NULL
  gbm_models <- NULL
  dl_models <- NULL
  count_aux <- 1
  glm_models[[count_aux]] <- list(
    auc_train = NULL,
    auc_validation = NULL,
    auc_test = NULL,
    target_hold_out = NULL,
    model = NULL
  )
  gbm_models[[count_aux]] <- list(
    auc_train = NULL,
    auc_validation = NULL,
    auc_test = NULL,
    target_hold_out = NULL,
    model = NULL
  )
  dl_models[[count_aux]] <- list(
    auc_train = NULL,
    auc_validation = NULL,
    auc_test = NULL,
    target_hold_out = NULL,
    model = NULL
  )
  hold_out[, y] <- as.factor(hold_out[y])
  split_db <- h2o.splitFrame(hold_out, ratios = c(0.2, 0.5))
  train <- split_db[[1]]
  validation <- split_db[[2]]
  test <- split_db[[3]]
  print("GLM:")
  glm_fit <- h2o::h2o.glm(
    x = x,
    y = y,
    training_frame = train,
    model_id = paste0("glm_fit_", count_aux),
    validation_frame = validation,
    family = "binomial",
    lambda_search = TRUE
  )
  best_glm_perf_train <- h2o::h2o.performance(
    model = glm_fit,
    newdata = train
  )
  best_glm_perf_validation <- h2o::h2o.performance(
    model = glm_fit,
    newdata = validation
  )
  best_glm_perf_test <- h2o::h2o.performance(
    model = glm_fit,
    newdata = test
  )
  glm_prediction <- h2o::h2o.predict(
    object = glm_fit,
    newdata = hold_out
  )
  
  glm_models[[count_aux]] <- list(
    auc_train = h2o.auc(best_glm_perf_train),
    auc_validation = h2o.auc(best_glm_perf_validation),
    auc_test = h2o.auc(best_glm_perf_test),
    accuracy_hold_out = 1 - (sum(glm_prediction$predict) / nrow(glm_prediction)),
    model = glm_fit
  )
  
  glm_fit@model_id <- "glm_fit"
  
  print("GBM:")
  gbm_fit <- h2o.grid("gbm",
                      x = x, y = y,
                      grid_id = paste0("gbm_fit_", count_aux),
                      training_frame = train,
                      validation_frame = validation,
                      ntrees = 100,
                      seed = 15917,
                      hyper_params = gbm_params,
                      search_criteria = search_criteria
  )
  
  gbm_fit_perf <- h2o.getGrid(
    grid_id = paste0("gbm_fit_", count_aux),
    sort_by = "auc",
    decreasing = TRUE
  )
  best_gbm <- h2o.getModel(gbm_fit_perf@model_ids[[1]])
  
  best_gbm_perf_train <- h2o::h2o.performance(
    model = best_gbm,
    newdata = train
  )
  best_gbm_perf_validation <- h2o::h2o.performance(
    model = best_gbm,
    newdata = validation
  )
  best_gbm_perf_test <- h2o::h2o.performance(
    model = best_gbm,
    newdata = test
  )
  gbm_prediction <- h2o::h2o.predict(
    object = best_gbm,
    newdata = hold_out
  )
  
  gbm_models[[count_aux]] <- list(
    auc_train = h2o.auc(best_gbm_perf_train),
    auc_validation = h2o.auc(best_gbm_perf_validation),
    auc_test = h2o.auc(best_gbm_perf_test),
    accuracy_hold_out = 1 - (sum(gbm_prediction$predict) / nrow(gbm_prediction)),
    model = best_gbm
  )
  
  gbm_fit@grid_id <- "gbm_fit"
  
  print("DL:")
  dl_fit <- h2o.grid("deeplearning",
                     x = x, y = y,
                     grid_id = paste0("dl_fit_", count_aux),
                     training_frame = train,
                     validation_frame = validation,
                     seed = 15917,
                     hidden = c(100, 100),
                     hyper_params = dl_params,
                     search_criteria = search_criteria
  )
  
  dl_fit_perf <- h2o.getGrid(
    grid_id = paste0("dl_fit_", count_aux),
    sort_by = "auc",
    decreasing = TRUE
  )
  
  best_dl <- h2o.getModel(dl_fit_perf@model_ids[[1]])
  
  best_dl_perf_train <- h2o::h2o.performance(
    model = best_dl,
    newdata = train
  )
  best_dl_perf_validation <- h2o::h2o.performance(
    model = best_dl,
    newdata = validation
  )
  best_dl_perf_test <- h2o::h2o.performance(
    model = best_dl,
    newdata = test
  )
  dl_prediction <- h2o::h2o.predict(
    object = best_dl,
    newdata = hold_out
  )
  
  dl_models[[count_aux]] <- list(
    auc_train = h2o.auc(best_dl_perf_train),
    auc_validation = h2o.auc(best_dl_perf_validation),
    auc_test = h2o.auc(best_dl_perf_test),
    accuracy_hold_out = 1 - (sum(dl_prediction$predict) / nrow(dl_prediction)),
    model = best_dl
  )
  
  dl_fit@grid_id <- "dl_fit"
  
  models <- NULL
  models$glm <- glm_models
  models$gbm <- gbm_models
  models$dl <- dl_models
  
  dataused <- NULL
  dataused$train <- train
  dataused$validation <- validation
  dataused$test <- test
  
  return(list(models = models, dataused = dataused))
}

evaluate_explored_models <- function(explored_models) {
  model_result <- NULL
  for (i in 1:length(explored_models)) {
    name_model <- names(explored_models)[i]
    model <- explored_models[[name_model]]
    auc_train <- NULL
    auc_validation <- NULL
    auc_test <- NULL
    accuracy_hold_out <- NULL
    for (j in 1:length(model)) {
      auc_train <- c(auc_validation, model[[j]]$auc_train)
      auc_validation <- c(auc_validation, model[[j]]$auc_validation)
      auc_test <- c(auc_test, model[[j]]$auc_test)
      accuracy_hold_out <- c(accuracy_hold_out, model[[j]]$accuracy_hold_out)
    }
    model_result_aux <- dplyr::data_frame(
      auc_train = auc_train,
      auc_validation = auc_validation,
      auc_test = auc_test,
      accuracy_hold_out = accuracy_hold_out
    )
    model_result_aux$name <- name_model
    model_result <- model_result %>%
      dplyr::bind_rows(model_result_aux)
  }
  
  summary_models_results <- model_result %>%
    dplyr::group_by(name) %>%
    dplyr::summarise_all(median) %>%
    dplyr::mutate(predictive_measure = auc_test * accuracy_hold_out) %>% 
    dplyr::arrange(desc(predictive_measure))
  
  return(summary_models_results)
}