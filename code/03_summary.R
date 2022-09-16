dat_o = readRDS(paste0(dir_data, '\\dat_o.RData'))
dat_i = readRDS(paste0(dir_data, '\\dat_i.RData'))

dat_o5 = readRDS(paste0(dir_data, '\\dat_o5.RData'))
dat_i5 = readRDS(paste0(dir_data, '\\dat_i5.RData'))


dat_i6 =
  dat_i %>%
  as_tibble() %>% 
  full_join(
    dat_i5 %>% 
      extract_nested_future_forecast(),
    by = c('ts_id' = 'ts_id',
           'migdate' = '.index')
  )

x = 
  dat_i6 %>% 
  filter(ts_id == 20 & between(year(migdate), 2011, 2021))

ggplot(x) +
  geom_line(aes(x = migdate,
                y = .value),
            color = 'red',
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = freq)) 


dat_o6 =
  dat_o %>%
  as_tibble() %>% 
  full_join(
    dat_o5 %>% 
      extract_nested_future_forecast(),
    by = c('ts_id' = 'ts_id',
           'migdate' = '.index')
  )

x = 
  dat_o6 %>% 
  filter(ts_id == 17 & between(year(migdate), 2011, 2021))

ggplot(x) +
  geom_line(aes(x = migdate,
                y = exp(.value)),
            color = 'red',
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = exp(freq))) 
