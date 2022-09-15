future_len = 36

# Load data ---------------------------------------------------------------

dat1 = read_feather(paste0(dir_data, '\\dat_raw_3ho.feather'))


# Data cleaning -----------------------------------------------------------

dat2 = dat1 %>% 
  transmute(
    # migration type
    type = code,
    # date of event
    migdate = migdate,
    # age of migrant when event happened
    agegrp = case_when(age <= 21 ~ '00-21',
                       age <= 39 ~ '22-39',
                       TRUE ~ '40-99'),
    # sending/receiving country 
    hostgrp = case_when(host == 'AUT' ~ 'AUT',
                        host == 'DEU' ~ 'DEU',
                        host %in% c('GBR', 'IRL') ~ 'GRB/IRL',
                        host %in% c('DNK', 'NLD', 'BEL',
                                    'LUX', 'SWE', 'NOR', 'FIN') ~ 'BENELUX/NORDIC',
                        TRUE ~ 'OTH'),
    # number of obs
    freq = 1
    )


# Outflow -----------------------------------------------------------------

# time series
dat_o = 
  dat2 %>% 
  filter(type == 'Outflow') %>% 
  select(-type) %>%
  # go to base level
  group_by(agegrp, 
           hostgrp) %>%
  # crate base level ts by summing up number of records
  summarise_by_time(.date_var = migdate,
                    .by = 'month',
                    freq = SUM(freq)) %>% 
  # fill in dates where no event happened by 1
  pad_by_time(.date_var = migdate,
              .by = 'month',
              .pad_value = 1,
              .start_date = '2010-01-01') %>% 
  # value becomes the log of averages daily events in a month
  mutate(freq = log(freq / days_in_month(migdate))) %>% 
  # transform to tsibble
  as_tsibble(key = c(agegrp, 
                     hostgrp),
             index = migdate) %>%
  # create aggregates by each level
  aggregate_key(agegrp * hostgrp, 
                freq = log(sum(exp(freq)))) %>% 
  # create ts identifier
  group_by(agegrp, hostgrp) %>% 
  mutate(ts_id = cur_group_id()) %>% 
  ungroup()

# nested ts object
dat_o2 = 
  dat_o %>%
  as_tibble() %>% 
  # before 2011 too much noise, afer 2019 covid test
  filter(migdate >= ymd('2011-01-01') & migdate <= ymd('2019-12-01')) %>% 
  select(-agegrp, -hostgrp) %>% 
  # forecast 24 month (2020, 2021)
  extend_timeseries(.id_var = ts_id,
                    .date_var = migdate,
                    .length_future = future_len) %>% 
  nest_timeseries(.id_var = ts_id,
                  .length_future = future_len,
                  .length_actual = 108) %>% 
  # train test split
  split_nested_timeseries(.length_test = 12)


# Inflow *-----------------------------------------------------------------

# time series
dat_i = 
  dat2 %>% 
  filter(type == 'Inflow') %>% 
  select(-type) %>%
  # go to base level
  group_by(agegrp, 
           hostgrp) %>%
  # crate base level ts by summing up number of records
  summarise_by_time(.date_var = migdate,
                    .by = 'month',
                    freq = SUM(freq)) %>% 
  # fill in dates where no event happened by 1
  pad_by_time(.date_var = migdate,
              .by = 'month',
              .pad_value = 1,
              .start_date = '2010-01-01') %>% 
  # value becomes the log of averages daily events in a month
  mutate(freq = log(freq / days_in_month(migdate))) %>% 
  # transform to tsibble
  as_tsibble(key = c(agegrp, 
                     hostgrp),
             index = migdate) %>%
  # create aggregates by each level
  aggregate_key(agegrp * hostgrp, 
                freq = log(sum(exp(freq)))) %>% 
  # create ts identifier
  group_by(agegrp, hostgrp) %>% 
  mutate(ts_id = cur_group_id()) %>% 
  ungroup()

# nested ts object
dat_i2 = 
  dat_i %>%
  as_tibble() %>% 
  # before 2011 too much noise, afer 2019 covid test
  filter(migdate >= ymd('2011-01-01') & migdate <= ymd('2019-12-01')) %>% 
  select(-agegrp, -hostgrp) %>% 
  # forecast 24 month (2020, 2021)
  extend_timeseries(.id_var = ts_id,
                    .date_var = migdate,
                    .length_future = future_len) %>% 
  nest_timeseries(.id_var = ts_id,
                  .length_future = future_len,
                  .length_actual = 108) %>% 
  # train test split
  split_nested_timeseries(.length_test = 12)


# Save data ---------------------------------------------------------------

write_feather(dat2, paste0(dir_data, '\\dat2.feather'))
saveRDS(dat_o, paste0(dir_data, '\\dat_o.RData'))
saveRDS(dat_o2, paste0(dir_data, '\\dat_o2.RData'))
saveRDS(dat_i, paste0(dir_data, '\\dat_i.RData'))
saveRDS(dat_i2, paste0(dir_data, '\\dat_i2.RData'))
