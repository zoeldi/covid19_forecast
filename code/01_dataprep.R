future_len = 24

# Load data ---------------------------------------------------------------

dat1 = read_feather(paste0(dir_data, '\\dat_raw_3honapos.feather'))


# Data cleaning -----------------------------------------------------------

dat2 = dat1 %>% 
  transmute(
    # migration type
    type = code,
    # date of event
    migdate = migdate,
    # age of migrant when event happened
    agegrp = case_when(age >= 17 & age <= 24 ~ '17-24',
                       age <= 30 ~ '25-34',
                       age <= 45 ~ '35-44',
                       age <= 65 ~ '45-65'),
    # sending/receiving country 
    hostgrp = case_when(host == 'AUT' ~ 'AUT',
                        host == 'DEU' ~ 'DEU',
                        host %in% c('GBR', 'IRL') ~ 'GRB/IRL',
                        host %in% c('DNK', 'NLD', 'BEL',
                                    'LUX', 'SWE', 'NOR', 'FIN') ~ 'BENELUX/NORDIC',
                        TRUE ~ 'OTH'),
    # number of obs
    freq = 1
    ) %>% 
  filter(!is.na(agegrp))


dat3 =
  dat2 %>% 
  # go to base level
  group_by(type, 
           agegrp, 
           hostgrp) %>%
  # crate base level ts by summing up number of records
  summarise_by_time(.date_var = migdate,
                    .by = 'month',
                    freq0 = SUM(freq)) %>%
  # fill in dates where no event happened by 0
  pad_by_time(.date_var = migdate,
              .by = 'month',
              .pad_value = 0,
              .start_date = '2010-01-01') %>% 
  # value becomes the log of averages daily events in a month + first difference
  mutate(freq = (freq0 + 1) / days_in_month(migdate),
         freqlog = log(freq),
         ts_id = cur_group_id()
         ) %>% 
  ungroup()
  

dat4 = 
  dat3 %>% 
  # before 2011 too much noise, afer 2019 covid test
  filter(migdate >= ymd('2011-01-01') & migdate <= ymd('2019-12-01')) %>% 
  # forecast 24 month (2020, 2021)
  extend_timeseries(.id_var = ts_id,
                    .date_var = migdate,
                    .length_future = future_len) %>% 
  nest_timeseries(.id_var = ts_id,
                  .length_future = future_len,
                  .length_actual = 108) %>% 
  # train test split
  split_nested_timeseries(.length_test = 12)
  

# Save ----------------------------------------------------------------------------------------
saveRDS(dat2, paste0(dir_data, '\\dat2.RDS'))
saveRDS(dat3, paste0(dir_data, '\\dat3.RDS'))
saveRDS(dat4, paste0(dir_data, '\\dat4.RDS'))
