dat_i = readRDS(paste0(dir_data, '\\dat_i.RData'))
dat_i5 = readRDS(paste0(dir_data, '\\dat_i5.RData'))


dat_i6 =
  dat_i %>%
  as_tibble() %>% 
  left_join(
    dat_i5 %>% 
      extract_nested_future_forecast(),
    by = c('ts_id' = 'ts_id',
           'migdate' = '.index')
  ) %>% 
  filter(migdate >= ymd('2011-01-01')) %>% 
  transmute(migdate,
            ts_id,
            agegrp,
            hostgrp,
            model_id = .model_id,
            model_desc = case_when(.model_desc == 'ARIMA W XGBOOST ERRORS' ~ 'ARIMA (XGB)',
                                   TRUE ~ .model_desc),
            key = .key,
            act_log = freq,
            pred_log = .value,
            pred_lo_log = .conf_lo,
            pred_hi_log = .conf_hi,
            act_abs_avg = exp(act_log),
            pred_abs_avg = exp(pred_log),
            pred_lo_abs_avg = exp(pred_lo_log),
            pred_hi_abs_avg = exp(pred_hi_log),
            act_abs_mth = days_in_month(migdate) * act_abs_avg,
            pred_abs_mth = days_in_month(migdate) * pred_abs_avg,
            pred_lo_abs_mth = days_in_month(migdate) * pred_lo_abs_avg,
            pred_hi_abs_mth = days_in_month(migdate) * pred_hi_abs_avg
  )


dat_i6_low =
  dat_i6 %>% 
  select(migdate, ts_id, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  mutate(pred_v1 = pred_abs_mth,
         pred_v1_lo = pred_lo_abs_mth,
         pred_v1_hi = pred_hi_abs_mth)

# dat_i6_low %>%
#   filter(ts_id == 9) %>%
#   ggplot() +
#   facet_grid(agegrp ~ hostgrp) +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()

dat_i6_mid_age = 
  dat_i6_low %>% 
  group_by(migdate, agegrp) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_i6_mid_age %>%
#   ggplot() +
#   facet_grid(agegrp ~ ., scales = 'free') +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()

dat_i6_mid_host = 
  dat_i6_low %>% 
  group_by(migdate, hostgrp) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_i6_mid_host %>%
#   ggplot() +
#   facet_grid(hostgrp ~ ., scales = 'free') +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()


dat_i6_high = 
  dat_i6_low %>% 
  group_by(migdate) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_i6_high %>%
#   ggplot() +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()


dat_i_v1 = 
  bind_rows(dat_i6_low,
            dat_i6_mid_age,
            dat_i6_mid_host,
            dat_i6_high)

saveRDS(dat_i_v1, paste0(dir_data, '\\dat_i_v1.RData'))

# Outflow -------------------------------------------------------------------------------------

dat_o = readRDS(paste0(dir_data, '\\dat_o.RData'))
dat_o5 = readRDS(paste0(dir_data, '\\dat_o5.RData'))


dat_o6 =
  dat_o %>%
  as_tibble() %>% 
  left_join(
    dat_o5 %>% 
      extract_nested_future_forecast(),
    by = c('ts_id' = 'ts_id',
           'migdate' = '.index')
  ) %>% 
  filter(migdate >= ymd('2011-01-01')) %>% 
  transmute(migdate,
            ts_id,
            agegrp,
            hostgrp,
            model_id = .model_id,
            model_desc = case_when(.model_desc == 'ARIMA W XGBOOST ERRORS' ~ 'ARIMA (XGB)',
                                   TRUE ~ .model_desc),
            key = .key,
            act_log = freq,
            pred_log = .value,
            pred_lo_log = .conf_lo,
            pred_hi_log = .conf_hi,
            act_abs_avg = exp(act_log),
            pred_abs_avg = exp(pred_log),
            pred_lo_abs_avg = exp(pred_lo_log),
            pred_hi_abs_avg = exp(pred_hi_log),
            act_abs_mth = days_in_month(migdate) * act_abs_avg,
            pred_abs_mth = days_in_month(migdate) * pred_abs_avg,
            pred_lo_abs_mth = days_in_month(migdate) * pred_lo_abs_avg,
            pred_hi_abs_mth = days_in_month(migdate) * pred_hi_abs_avg
  )


dat_o6_low =
  dat_o6 %>% 
  select(migdate, ts_id, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  mutate(pred_v1 = pred_abs_mth,
         pred_v1_lo = pred_lo_abs_mth,
         pred_v1_hi = pred_hi_abs_mth)

# dat_o6_low %>%
#   filter(ts_id == 9) %>%
#   ggplot() +
#   facet_grid(agegrp ~ hostgrp) +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()

dat_o6_mid_age = 
  dat_o6_low %>% 
  group_by(migdate, agegrp) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_o6_mid_age %>%
#   ggplot() +
#   facet_grid(agegrp ~ ., scales = 'free') +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()

dat_o6_mid_host = 
  dat_o6_low %>% 
  group_by(migdate, hostgrp) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_o6_mid_host %>%
#   ggplot() +
#   facet_grid(hostgrp ~ ., scales = 'free') +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()


dat_o6_high = 
  dat_o6_low %>% 
  group_by(migdate) %>% 
  summarise(act_abs_mth = sum(act_abs_mth),
            pred_v1 = sum(pred_v1),
            pred_v1_lo = sum(pred_v1_lo),
            pred_v1_hi = sum(pred_v1_hi))

# dat_o6_high %>%
#   ggplot() +
#   geom_ribbon(aes(x = migdate,
#                   ymin = pred_v1_lo,
#                   ymax = pred_v1_hi),
#               fill = colorblind$red,
#               alpha = 0.3) +
#   geom_line(aes(x = migdate,
#                 y = pred_v1),
#             color = colorblind$red) +
#   geom_line(aes(x = migdate,
#                 y = act_abs_mth)) +
#   theme_bw()


dat_o_v1 = 
  bind_rows(dat_o6_low,
            dat_o6_mid_age,
            dat_o6_mid_host,
            dat_o6_high)

saveRDS(dat_o_v1, paste0(dir_data, '\\dat_o_v1.RData'))
