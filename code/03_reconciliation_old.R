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
            agegrp = case_when(is_aggregated(agegrp) ~ 'Aggregated',
                               TRUE ~ as.character(agegrp)),
            hostgrp = case_when(is_aggregated(hostgrp) ~ 'Aggregated',
                                TRUE ~ as.character(hostgrp)),
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
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp != 'Aggregated' & hostgrp != 'Aggregated') %>% 
  mutate(pred_v1 = pred_abs_mth,
         pred_v1_lo = pred_lo_abs_mth,
         pred_v1_hi = pred_hi_abs_mth)

dat_i6_mid_age = 
  dat_i6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated') %>% 
  left_join(dat_i6_low %>% 
              group_by(migdate, agegrp) %>% 
              summarise(pred_abs_mth_rec = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec = sum(pred_hi_abs_mth)),
            by = c('migdate', 'agegrp')) %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec) / 2,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec) / 2,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec) / 2)

dat_i6_mid_host = 
  dat_i6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated') %>% 
  left_join(dat_i6_low %>% 
              group_by(migdate, hostgrp) %>% 
              summarise(pred_abs_mth_rec = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec = sum(pred_hi_abs_mth)),
            by = c('migdate', 'hostgrp')) %>% 
  ungroup() %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec) / 2,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec) / 2,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec) / 2)


dat_i6_high = 
  dat_i6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated') %>% 
  left_join(dat_i6_low %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_l = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec_l = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec_l = sum(pred_hi_abs_mth)),
            by = c('migdate')) %>% 
  left_join(dat_i6_mid_age %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_ma = sum(pred_v1),
                        pred_lo_abs_mth_rec_ma = sum(pred_v1_lo),
                        pred_hi_abs_mth_rec_ma = sum(pred_v1_hi)),
            by = c('migdate'))  %>% 
  left_join(dat_i6_mid_host %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_mh = sum(pred_v1),
                        pred_lo_abs_mth_rec_mh = sum(pred_v1_lo),
                        pred_hi_abs_mth_rec_mh = sum(pred_v1_hi)),
            by = c('migdate')) %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec_l + pred_abs_mth_rec_ma + pred_abs_mth_rec_mh) / 4,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec_l + pred_lo_abs_mth_rec_ma + pred_lo_abs_mth_rec_mh) / 4,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec_l + pred_hi_abs_mth_rec_ma + pred_hi_abs_mth_rec_mh) / 4)


dat_i_v1 = 
  dat_i6_low %>% 
  bind_rows(dat_i6_mid_age,
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
            agegrp = case_when(is_aggregated(agegrp) ~ 'Aggregated',
                               TRUE ~ as.character(agegrp)),
            hostgrp = case_when(is_aggregated(hostgrp) ~ 'Aggregated',
                                TRUE ~ as.character(hostgrp)),
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
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp != 'Aggregated' & hostgrp != 'Aggregated') %>% 
  mutate(pred_v1 = pred_abs_mth,
         pred_v1_lo = pred_lo_abs_mth,
         pred_v1_hi = pred_hi_abs_mth)

dat_o6_mid_age = 
  dat_o6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated') %>% 
  left_join(dat_o6_low %>% 
              group_by(migdate, agegrp) %>% 
              summarise(pred_abs_mth_rec = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec = sum(pred_hi_abs_mth)),
            by = c('migdate', 'agegrp')) %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec) / 2,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec) / 2,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec) / 2)

dat_o6_mid_host = 
  dat_o6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated') %>% 
  left_join(dat_o6_low %>% 
              group_by(migdate, hostgrp) %>% 
              summarise(pred_abs_mth_rec = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec = sum(pred_hi_abs_mth)),
            by = c('migdate', 'hostgrp')) %>% 
  ungroup() %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec) / 2,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec) / 2,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec) / 2)


dat_o6_high = 
  dat_o6 %>% 
  select(migdate, agegrp, hostgrp, model_desc, key, act_abs_mth, pred_abs_mth,
         pred_lo_abs_mth, pred_hi_abs_mth) %>%
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated') %>% 
  left_join(dat_o6_low %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_l = sum(pred_abs_mth),
                        pred_lo_abs_mth_rec_l = sum(pred_lo_abs_mth),
                        pred_hi_abs_mth_rec_l = sum(pred_hi_abs_mth)),
            by = c('migdate')) %>% 
  left_join(dat_o6_mid_age %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_ma = sum(pred_v1),
                        pred_lo_abs_mth_rec_ma = sum(pred_v1_lo),
                        pred_hi_abs_mth_rec_ma = sum(pred_v1_hi)),
            by = c('migdate'))  %>% 
  left_join(dat_o6_mid_host %>% 
              group_by(migdate) %>% 
              summarise(pred_abs_mth_rec_mh = sum(pred_v1),
                        pred_lo_abs_mth_rec_mh = sum(pred_v1_lo),
                        pred_hi_abs_mth_rec_mh = sum(pred_v1_hi)),
            by = c('migdate')) %>%
  mutate(pred_v1 = (pred_abs_mth + pred_abs_mth_rec_l + pred_abs_mth_rec_ma + pred_abs_mth_rec_mh) / 4,
         pred_v1_lo = (pred_lo_abs_mth + pred_lo_abs_mth_rec_l + pred_lo_abs_mth_rec_ma + pred_lo_abs_mth_rec_mh) / 4,
         pred_v1_hi = (pred_hi_abs_mth + pred_hi_abs_mth_rec_l + pred_hi_abs_mth_rec_ma + pred_hi_abs_mth_rec_mh) / 4)



dat_o_v1 = 
  dat_o6_low %>% 
  bind_rows(dat_o6_mid_age,
            dat_o6_mid_host,
            dat_o6_high)

saveRDS(dat_o_v1, paste0(dir_data, '\\dat_o_v1.RData'))
