

# Load data ---------------------------------------------------------------

dat_o2 = readRDS(paste0(dir_data, '\\dat_o2.RData'))
dat_i2 = readRDS(paste0(dir_data, '\\dat_i2.RData'))


# Feature engine ----------------------------------------------------------

# Recipe for statistical models (only time column)
recipe_o1 = 
  recipe(freq ~ migdate, 
         data = extract_nested_train_split(dat_o2))

recipe_i1 = 
  recipe(freq ~ migdate, 
         data = extract_nested_train_split(dat_i2))

# Recipe for machine learning models (ts signaure, base)
recipe_o2 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_o2)) %>%
  # creates ts features from date column
  step_timeseries_signature(migdate) %>%
  # removes constant columns
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  # one-hot dummy encoding
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

recipe_i2 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_i2)) %>%
  # creates ts features from date column
  step_timeseries_signature(migdate) %>%
  # removes constant columns
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  # one-hot dummy encoding
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

# Recipe for machine learning models (ts signature, fourier series K=1)
recipe_o3 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_o2)) %>% 
  step_timeseries_signature(migdate) %>%
  # Creates fseries where K = 1
  step_fourier(migdate, 
               period = c(12/4), 
               K = 1) %>% 
  step_rm(migdate) %>%
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

recipe_i3 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_i2)) %>% 
  step_timeseries_signature(migdate) %>%
  # Creates fseries where K = 1
  step_fourier(migdate, 
               period = c(12/4), 
               K = 1) %>% 
  step_rm(migdate) %>%
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Recipe for machine learning models (ts signature, fourier series K=2)
recipe_o4 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_o2)) %>% 
  step_timeseries_signature(migdate) %>%
  step_fourier(migdate, 
               period = c(12/4), 
               K = 2) %>% 
  step_rm(migdate) %>%
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

recipe_i4 =
  recipe(freq ~ .,
         data = extract_nested_train_split(dat_i2)) %>% 
  step_timeseries_signature(migdate) %>%
  step_fourier(migdate, 
               period = c(12/4), 
               K = 2) %>% 
  step_rm(migdate) %>%
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

#bake(prep(recipe_o2), new_data = extract_nested_train_split(dat_o2))


# Models ------------------------------------------------------------------

# ARIMA
mod_arima = 
  arima_reg(mode = 'regression') %>% 
  set_engine('auto_arima')

# PROPHET
mod_prophet = 
  prophet_reg(mode = 'regression',
              seasonality_yearly = T) %>% 
  set_engine('prophet')

# ARIMA-BOOST
mod_xgb = 
  tibble(expand_grid(
    tree_depth = c(
      3,
      6,
      10
    ),
    learn_rate = c(
      0.010,
      0.025,
      0.050,
      0.100,
      0.125,
      0.150,
      0.175,
      0.200,
      0.300
    ),
    sample_size = c(
      0.5
    ),
    trees = c(
      10, 
      20, 
      30
    ),
    stop_iter = c(3)
    )
  ) %>% 
  create_model_grid(f_model_spec = arima_boost,
                    engine_name = 'auto_arima_xgboost',
                    mode = 'regression')
mod_xgb = mod_xgb$.models


# Workflows ---------------------------------------------------------------

wflw_arima_o1 =
  workflow() %>% 
  add_model(mod_arima) %>% 
  add_recipe(recipe_o1)

wflw_arima_i1 =
  workflow() %>% 
  add_model(mod_arima) %>% 
  add_recipe(recipe_i1)

wflw_prophet_o1 =
  workflow() %>% 
  add_model(mod_prophet) %>% 
  add_recipe(recipe_o1)

wflw_prophet_i1 = 
  workflow() %>% 
  add_model(mod_prophet) %>% 
  add_recipe(recipe_i1)

wflw_xgb_o1 =
  workflow_set(preproc = list(recipe_o2),
               models = mod_xgb,
               cross = TRUE)
wflw_xgb_o1 =  sapply(wflw_xgb_o1[[2]], function(i) i[[1]])
  
wflw_xgb_i1 =
  workflow_set(preproc = list(recipe_i2),
               models = mod_xgb,
               cross = TRUE)

wflw_xgb_i1 =  sapply(wflw_xgb_i1[[2]], function(i) i[[1]])


# Fitting -----------------------------------------------------------------

num_cores = parallel::detectCores()


parallel_start(num_cores)

dat_o3 = 
  modeltime_nested_fit(nested_data = dat_o2,
                       model_list = append(wflw_xgb_o1, 
                                           c(list(wflw_arima_o1),
                                           list(wflw_prophet_o1))),
                       control = control_nested_fit(allow_par = TRUE,
                                                    cores = as.numeric(num_cores),
                                                    verbose = TRUE))

dat_i3 = 
  modeltime_nested_fit(nested_data = dat_i2,
                       model_list = append(wflw_xgb_i1, 
                                           c(list(wflw_arima_i1),
                                             list(wflw_prophet_i1))),
                       control = control_nested_fit(allow_par = TRUE,
                                                    cores = as.numeric(num_cores),
                                                    verbose = TRUE))
parallel_stop()

dat_o4 =
  dat_o3 %>%
  modeltime_nested_select_best(metric = "mae",
                               minimize = TRUE, 
                               filter_test_forecasts = TRUE)
dat_i4 =
  dat_i3 %>%
  modeltime_nested_select_best(metric = "mase",
                               minimize = TRUE, 
                               filter_test_forecasts = TRUE)

dat_o5 = 
  dat_o4 %>%
  modeltime_nested_refit(control = control_nested_refit(verbose = TRUE))
dat_i5 = 
  dat_i4 %>%
  modeltime_nested_refit(control = control_nested_refit(verbose = TRUE))

dat_o5 %>%
  extract_nested_future_forecast() %>%
  group_by(ts_id) %>%
  plot_modeltime_forecast(.interactive = FALSE,
                          .facet_ncol  = 5,
                          .conf_interval_show = T)
dat_i5 %>%
  extract_nested_future_forecast() %>%
  group_by(ts_id) %>%
  plot_modeltime_forecast(.interactive = FALSE,
                          .facet_ncol  = 5,
                          .conf_interval_show = T)


saveRDS(dat_o3, paste0(dir_data, '\\dat_o3.RData'))
saveRDS(dat_o4, paste0(dir_data, '\\dat_o4.RData'))
saveRDS(dat_o5, paste0(dir_data, '\\dat_o5.RData'))


saveRDS(dat_i3, paste0(dir_data, '\\dat_i3.RData'))
saveRDS(dat_i4, paste0(dir_data, '\\dat_i4.RData'))
saveRDS(dat_i5, paste0(dir_data, '\\dat_i5.RData'))
