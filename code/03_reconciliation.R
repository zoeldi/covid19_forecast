dat3 = readRDS(paste0(dir_data, '\\dat3.RDS'))
dat7 = readRDS(paste0(dir_data, '\\dat7.RDS'))


# Base level ----------------------------------------------------------------------------------

dat8 = 
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

# dat8 = 
#   dat3 %>% 
#   left_join(
#     dat7 %>% extract_nested_future_forecast(),
#     by = c('ts_id' = 'ts_id',
#            'migdate' = '.index')
#   ) %>% 
#   filter(migdate >= '2011-01-01') %>% 
#   mutate(actual = exp(freqlog) * days_in_month(migdate),
#          .conf_lo = ifelse(is.na(.conf_lo), freqlog, .conf_lo),
#          .conf_hi = ifelse(is.na(.conf_hi), freqlog, .conf_hi)) %>% 
#   mutate(
#     pred_mi = (lambda * .value + 1) ^ (1/lambda) * days_in_month(migdate),
#     pred_lo = (lambda * .conf_lo + 1) ^ (1/lambda) * days_in_month(migdate),
#     pred_hi = (lambda * .conf_hi + 1) ^ (1/lambda) * days_in_month(migdate),
#     pred_mi_perc = (actual / pred_mi - 1),
#     pred_lo_perc = (actual / pred_lo - 1),
#     pred_hi_perc = (actual / pred_hi - 1)
#   ) %>% 
#   group_by(ts_id) %>% 
#   mutate(
#     pred_mi_cum = cumsum(pred_mi),
#     pred_lo_cum = cumsum(pred_lo),
#     pred_hi_cum = cumsum(pred_hi),
#     actual_cum = cumsum(actual)
#   )

## Inflow #############

### Abs ###############
plt_base_in_abs = 
  dat8 %>% 
  filter(type == 'Inflow') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi,
                color = .model_desc)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$green,
                                colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c('black',
                               colorblind$green,
                               colorblind$lblue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_in_abs

ggsave(plt_base_in_abs,
       filename = paste0(dir_out, '\\plt_base_in_abs.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)

### Perc ###############
plt_base_in_perc = 
  dat8 %>% 
  filter(type == 'Inflow' & migdate >= '2020-01-01') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo_perc,
                  ymax = pred_hi_perc),
              alpha = 0.2,
              fill = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc),
            color = colorblind$lblue) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc),
             color = colorblind$lblue) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_in_perc

ggsave(plt_base_in_perc,
       filename = paste0(dir_out, '\\plt_base_in_perc.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)

### Cum ################
plt_base_in_cum = 
  dat8 %>% 
  filter(type == 'Inflow' & migdate >= '2020-01-01') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo_cum,
                  ymax = pred_hi_cum),
              alpha = 0.2,
              fill = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = pred_mi_cum),
            color = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = actual_cum)) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_in_cum

ggsave(plt_base_in_cum,
       filename = paste0(dir_out, '\\plt_base_in_cum.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)


## Outflow #############

### Abs ###############
plt_base_out_abs = 
  dat8 %>% 
  filter(type == 'Outflow') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi,
                color = .model_desc)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$green,
                                colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c('black',
                               colorblind$green,
                               colorblind$lblue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_out_abs

ggsave(plt_base_out_abs,
       filename = paste0(dir_out, '\\plt_base_out_abs.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)

### Perc ###############
plt_base_out_perc = 
  dat8 %>% 
  filter(type == 'Outflow' & migdate >= '2020-01-01') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo_perc,
                  ymax = pred_hi_perc),
              alpha = 0.2,
              fill = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc),
            color = colorblind$lblue) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc),
             color = colorblind$lblue) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_out_perc

ggsave(plt_base_out_perc,
       filename = paste0(dir_out, '\\plt_base_out_perc.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)

### Cum ################
plt_base_in_cum = 
  dat8 %>% 
  filter(type == 'Inflow' & migdate >= '2020-01-01') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo_cum,
                  ymax = pred_hi_cum),
              alpha = 0.2,
              fill = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = pred_mi_cum),
            color = colorblind$lblue) +
  geom_line(aes(x = migdate,
                y = actual_cum)) +
  theme_bw() + 
  theme(legend.position = 'bottom'); plt_base_in_cum

ggsave(plt_base_in_cum,
       filename = paste0(dir_out, '\\plt_base_in_cum.png'),
       width = 40,
       units = 'cm',
       dpi = 1000)

