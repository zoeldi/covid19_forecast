

# Load ----------------------------------------------------------------------------------------

dat4 = readRDS(paste0(dir_data, '\\dat4.RDS'))


# Recipe --------------------------------------------------------------------------------------

recipe1 = 
  recipe(freqlog ~ migdate, 
         data = extract_nested_train_split(dat4))

recipe2 =
  recipe(freqlog ~ .,
         data = extract_nested_train_split(dat4)) %>%
  # creates ts features from date column
  step_timeseries_signature(migdate) %>%
  # removes constant columns
  step_zv(all_predictors()) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts"), contains('day'), 
          contains('week')) %>% 
  step_rm(freq0, freqbc, freq, freqdiff) %>% 
  # one-hot dummy encoding
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_normalize(matches("(index.num$)|(_year$)"))

#bake(prep(recipe2), new_data = extract_nested_train_split(dat4)) %>% View()
# Model ---------------------------------------------------------------------------------------

# PROPHET
mod_prophet = 
  tibble(
    expand_grid(
      growth = c('linear'),
      changepoint_num = 25,
      changepoint_range = c(0.90),
      seasonality_yearly = TRUE,
      seasonality_weekly = FALSE,
      seasonality_daily = FALSE,
      season = c('additive'),
      prior_scale_changepoints = c(0.05, 0.1, 0.2, 0.5),
      prior_scale_seasonality = c(0.1, 0.5, 1, 5, 10)
    )
  ) %>% 
  create_model_grid(f_model_spec = prophet_reg,
                    engine_name = 'prophet',
                    mode = 'regression'); mod_prophet = mod_prophet$.models

# ARIMA
mod_arima = 
  arima_reg(mode = 'regression') %>% 
  set_engine('auto_arima')

# EXPSMOOTH
mod_expsmooth = 
  exp_smoothing() %>% 
  set_engine('ets')

# ARIMA_XGB
mod_arima_xgb = 
  tibble(
    expand_grid(
      learn_rate = c(0.2, 0.1, 0.3),
      min_n = 2
    )
  ) %>% 
  create_model_grid(f_model_spec = arima_boost,
                    engine_name = 'auto_arima_xgboost',
                    mode = 'regression'); mod_arima_xgb = mod_arima_xgb$.models

# PROPHET-XGB
mod_prophet_xgb = 
  tibble(
    expand_grid(
      growth = c('linear'),
      changepoint_num = 25,
      changepoint_range = c(0.90),
      seasonality_yearly = TRUE,
      seasonality_weekly = FALSE,
      seasonality_daily = FALSE,
      season = c('additive'),
      prior_scale_changepoints = c(0.05, 0.1, 0.2, 0.5),
      prior_scale_seasonality = c(0.1, 0.5, 1, 5, 10),
      learn_rate = c(0.1, 0.2, 0.3),
      min_n = 2
    )
  ) %>% 
  create_model_grid(f_model_spec = prophet_boost,
                    engine_name = 'prophet_xgboost',
                    mode = 'regression'); mod_prophet_xgb = mod_prophet_xgb$.models

# TBATS
mod_tbats = 
  seasonal_reg() %>% 
  set_engine('tbats',
             use.box.cox = FALSE)

# Workflow ------------------------------------------------------------------------------------

# PROPHET
wflw_prophet =
  workflow_set(preproc = list(recipe1),
               models = mod_prophet,
               cross = TRUE)
wflw_prophet =  sapply(wflw_prophet[[2]], function(i) i[[1]])

# ARIMA
wflw_arima =
  workflow() %>% 
  add_model(mod_arima) %>% 
  add_recipe(recipe1)

# EXPSMOOTH
wflw_ets = 
  workflow() %>% 
  add_model(mod_expsmooth) %>% 
  add_recipe(recipe1)

# ARIMA-XGB
wflw_arima_xgb =
  workflow_set(preproc = list(recipe2),
               models = mod_arima_xgb,
               cross = TRUE)
wflw_arima_xgb =  sapply(wflw_arima_xgb
                         [[2]], function(i) i[[1]])

# PROPHET-XGB
wflw_prophet_xgb =
  workflow_set(preproc = list(recipe2),
               models = mod_prophet_xgb,
               cross = TRUE)
wflw_prophet_xgb =  sapply(wflw_prophet_xgb
                           [[2]], function(i) i[[1]])

# TBATS
wflw_tbats = 
  workflow() %>% 
  add_model(mod_tbats) %>% 
  add_recipe(recipe1)


# Fit -----------------------------------------------------------------------------------------
set.seed(07635)
# Set num of cores used to fitting
num_cores = parallel::detectCores()

# Start clusters
parallel_start(num_cores)

dat5 = 
  modeltime_nested_fit(nested_data = dat4,
                       model_list = 
                         list(wflw_arima,
                              wflw_tbats, 
                              wflw_ets) %>%
                         append(wflw_arima_xgb) %>% 
                         append(wflw_prophet_xgb) %>% 
                         append(wflw_prophet),
                       control = control_nested_fit(allow_par = TRUE,
                                                    cores = as.numeric(num_cores),
                                                    verbose = TRUE))
parallel_stop()


# Validate ------------------------------------------------------------------------------------

# All models
dat5 %>% 
  extract_nested_test_forecast() %>%
  group_by(ts_id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 5,
    .interactive = FALSE,
    .conf_interval_show = FALSE
  )

# Model rank
dat6 =
  dat5 %>%
  modeltime_nested_select_best(metric = "rmse",
                               minimize = TRUE, 
                               filter_test_forecasts = TRUE)

# All models
dat6 %>% 
  extract_nested_test_forecast() %>%
  group_by(ts_id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 5,
    .interactive = FALSE,
    .conf_interval_show = FALSE
  )

# Refit
dat7 = 
  dat6 %>%
  modeltime_nested_refit(control = control_nested_refit(verbose = TRUE))

# Validate
dat7 %>%
  extract_nested_future_forecast() %>%
  group_by(ts_id) %>%
  plot_modeltime_forecast(.interactive = FALSE,
                          .facet_ncol  = 5,
                          .conf_interval_show = T)

saveRDS(dat5, paste0(dir_data, '\\dat5.RDS'))
saveRDS(dat6, paste0(dir_data, '\\dat6.RDS'))
saveRDS(dat7, paste0(dir_data, '\\dat7.RDS'))
