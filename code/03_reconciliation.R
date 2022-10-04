dat3 = readRDS(paste0(dir_data, '\\dat3.RDS'))
dat7 = readRDS(paste0(dir_data, '\\dat7.RDS'))


# Base level ----------------------------------------------------------------------------------

dat_base = 
  dat3 %>% 
  left_join(
    dat7 %>% extract_nested_future_forecast(),
    by = c('ts_id' = 'ts_id',
           'migdate' = '.index')
  ) %>% 
  filter(migdate >= '2011-01-01') %>% 
  mutate(actual = exp(freqlog) * days_in_month(migdate),
         .conf_lo = ifelse(is.na(.conf_lo), freqlog, .conf_lo),
         .conf_hi = ifelse(is.na(.conf_hi), freqlog, .conf_hi)) %>% 
  mutate(
    pred_mi = exp(.value) * days_in_month(migdate),
    pred_lo = exp(.conf_lo) * days_in_month(migdate),
    pred_hi = exp(.conf_hi) * days_in_month(migdate),
    pred_mi_perc = (actual / pred_mi - 1),
    pred_lo_perc = (actual / pred_lo - 1),
    pred_hi_perc = (actual / pred_hi - 1)
  ) %>% 
  group_by(ts_id) %>% 
  mutate(
    pred_mi_cum = cumsum(pred_mi),
    pred_lo_cum = cumsum(pred_lo),
    pred_hi_cum = cumsum(pred_hi),
    actual_cum = cumsum(actual)
    )

dat_recon = 
  dat8 %>% 
  group_by(type, agegrp, hostgrp, migdate, .key) %>% 
  summarise(freq0 = sum(freq0),
            actual = sum(actual),
            pred_mi = sum(pred_mi),
            pred_lo = sum(pred_lo),
            pred_hi = sum(pred_hi),
            pred_mi_cum = sum(pred_mi_cum),
            pred_lo_cum = sum(pred_lo_cum),
            pred_hi_cum = sum(pred_hi_cum)) %>% 
  bind_rows(
    dat8 %>% 
      group_by(type, agegrp, hostgrp = 'Aggregated', migdate, .key) %>% 
      summarise(freq0 = sum(freq0),
                actual = sum(actual),
                pred_mi = sum(pred_mi),
                pred_lo = sum(pred_lo),
                pred_hi = sum(pred_hi),
                pred_mi_cum = sum(pred_mi_cum),
                pred_lo_cum = sum(pred_lo_cum),
                pred_hi_cum = sum(pred_hi_cum)),
    dat8 %>% 
      group_by(type, agegrp = 'Aggregated', hostgrp, migdate, .key) %>% 
      summarise(freq0 = sum(freq0),
                actual = sum(actual),
                pred_mi = sum(pred_mi),
                pred_lo = sum(pred_lo),
                pred_hi = sum(pred_hi),
                pred_mi_cum = sum(pred_mi_cum),
                pred_lo_cum = sum(pred_lo_cum),
                pred_hi_cum = sum(pred_hi_cum)),
    dat8 %>% 
      group_by(type, agegrp = 'Aggregated', hostgrp = 'Aggregated', migdate, .key) %>% 
      summarise(freq0 = sum(freq0),
                actual = sum(actual),
                pred_mi = sum(pred_mi),
                pred_lo = sum(pred_lo),
                pred_hi = sum(pred_hi),
                pred_mi_cum = sum(pred_mi_cum),
                pred_lo_cum = sum(pred_lo_cum),
                pred_hi_cum = sum(pred_hi_cum))
  )


# Save ----------------------------------------------------------------------------------------

saveRDS(dat_base, paste0(dir_data, '\\dat_base.RDS'))
saveRDS(dat_recon, paste0(dir_data, '\\dat_recon.RDS'))
