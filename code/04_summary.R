
# Base-level inflow by model ------------------------------------------------------------------

## Absolute -----------
(plt_base_inflow_abs =
   dat_base %>% 
   filter(type == 'Inflow') %>% 
   ggplot() +
   facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
   geom_ribbon(aes(x = migdate,
                   ymin = pred_lo,
                   ymax = pred_hi,
                   fill = .model_desc),
               alpha = 0.2) +
   geom_line(aes(x = migdate,
                 y = pred_mi,
                 color = .model_desc), size = 0.8) +
   geom_line(aes(x = migdate,
                 y = actual), size = 0.8) +
   # scale_color_manual(values = c('black',
   #                               colorblind$green,
   #                               colorblind$lblue,
   #                               colorblind$red),
   #                    name = 'Model family') +
   # scale_fill_manual(values = c('black',
   #                              colorblind$green,
   #                              colorblind$lblue,
   #                              colorblind$red),
   #                   name = 'Model family') +
   theme_bw() + 
   theme(legend.position = 'bottom',
         axis.title.x = element_blank()) +
   ylab('Number of Hngarian immigrants')
)


## Percentage -----------
(plt_base_inflow_perc =
  dat_base %>% 
  filter(type == 'Inflow' & migdate >= ymd('2020-01-01')) %>% 
  rowwise() %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100,
         issignificant = case_when(pred_hi_perc <= 100 & pred_lo_perc >= 100 ~ 'Non-Significant', TRUE ~ 'Significant')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = .model_desc)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = .model_desc),
             size = 2) +
  # scale_color_manual(values = c(colorblind$green,
  #                               colorblind$lblue,
  #                               colorblind$red),
  #                    name = 'Model family') +
  # scale_fill_manual(values = c(colorblind$green,
  #                              colorblind$lblue,
  #                              colorblind$red),
  #                   name = 'Model family') +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Deviation of actual immigration from predicted (%)')
)



## Cumsum -----------
plt_base_inflow_cumsum = 
  dat_base %>% 
  filter(type == 'Inflow' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_lo_cum * 100,
                  ymax = actual_cum / pred_hi_cum * 100,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = .model_desc)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = .model_desc)) +
  # scale_color_manual(values = c(colorblind$green,
  #                               colorblind$lblue,
  #                               colorblind$red),
  #                    name = 'Model family') +
  # scale_fill_manual(values = c(colorblind$green,
  #                              colorblind$lblue,
  #                              colorblind$red),
  #                   name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Cumsum')




# Base-level outflow by model -----------------------------------------------------------------

## Absolute ---------------------
(plt_base_outflow_abs =
   dat_base %>% 
   filter(type == 'Outflow') %>% 
   ggplot() +
   facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
   geom_ribbon(aes(x = migdate,
                   ymin = pred_lo,
                   ymax = pred_hi,
                   fill = .model_desc),
               alpha = 0.2) +
   geom_line(aes(x = migdate,
                 y = pred_mi,
                 color = .model_desc), size = 0.8) +
   geom_line(aes(x = migdate,
                 y = actual), size = 0.8) +
   # scale_color_manual(values = c('black',
   #                               colorblind$green,
   #                               colorblind$lblue,
   #                               colorblind$red),
   #                    name = 'Model family') +
   # scale_fill_manual(values = c('black',
   #                              colorblind$green,
   #                              colorblind$lblue,
   #                              colorblind$red),
   #                   name = 'Model family') +
   theme_bw() + 
   theme(legend.position = 'bottom',
         axis.title.x = element_blank()) +
   ylab('Number of Hngarian emigrants')
)



## Percentage -----------
plt_base_outflow_perc = 
  dat_base %>% 
  filter(type == 'Outflow' & migdate >= ymd('2020-01-01')) %>% 
  rowwise() %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100,
         issignificant = case_when(pred_hi_perc <= 100 & pred_lo_perc >= 100 ~ 'Non-Significant', TRUE ~ 'Significant')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = .model_desc)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = .model_desc),
             size = 2) +
  # scale_color_manual(values = c(colorblind$green,
  #                               colorblind$lblue,
  #                               colorblind$red),
  #                    name = 'Model family') +
  # scale_fill_manual(values = c(colorblind$green,
  #                              colorblind$lblue,
  #                              colorblind$red),
  #                   name = 'Model family') +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Deviation of actual emigration from predicted (%)')




## Cumsum -----------
plt_base_outflow_cumsum = 
  dat_base %>% 
  filter(type == 'Outflow' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, ts_id, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_hi_cum * 100,
                  ymax = actual_cum / pred_lo_cum * 100,
                  fill = .model_desc),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = .model_desc)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = .model_desc),
             size = 2) +
  # scale_color_manual(values = c(colorblind$green,
  #                               colorblind$lblue,
  #                               colorblind$red),
  #                    name = 'Model family') +
  # scale_fill_manual(values = c(colorblind$green,
  #                              colorblind$lblue,
  #                              colorblind$red),
  #                   name = 'Model family') +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Cumsum')





# Aggregated by age ---------------------------------------------------------------------------

## Absolute ---------------------
plt_age_abs = 
  dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(agegrp, type, sep = ' | ') ~ ., ncol =2, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = .key),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = .key)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$lblue),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$lblue),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')




## Percentage -----------
plt_age_perc = 
  dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100) %>% 
  ggplot() +
  facet_wrap(paste(type, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 3) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Deviation of actual migration from predicted (%)')




## Cumsum -----------
plt_age_cumsum = 
  dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(paste(agegrp, type, sep = ' | ') ~ ., scales = 'free', ncol = 2) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_hi_cum * 100,
                  ymax = actual_cum / pred_lo_cum * 100,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Cumsum')





# Aggregated by host -------------------------------------------------------------------------

## Absolute ---------------------
plt_host_abs = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, type, sep = ' | ') ~ ., ncol =2, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = .key),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = .key), size = 1) +
  geom_line(aes(x = migdate,
                y = actual), size = 1) +
  scale_color_manual(values = c('black',
                                colorblind$lblue),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$lblue),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')




## Percentage -----------
plt_host_perc = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, type, sep = ' | ') ~ ., scales = 'free', ncol = 2) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Deviation of actual migration from predicted (%)')




## Cumsum -----------
plt_host_cumsum = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, type, sep = ' | ') ~ ., scales = 'free', ncol = 2) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_hi_cum * 100,
                  ymax = actual_cum / pred_lo_cum * 100,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Cumsum')





# Aggregated by type -------------------------------------------------------------------------

## Absolute ---------------------
plt_aggregated_abs = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(type) ~ ., ncol = 1, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = .key),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = .key), size = 1) +
  geom_line(aes(x = migdate,
                y = actual), size = 1) +
  scale_color_manual(values = c('black',
                                colorblind$lblue),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$lblue),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')




## Percentage -----------
plt_aggregated_perc = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100) %>% 
  ggplot() +
  facet_wrap(type ~ ., scales = 'free', ncol = 1) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Deviation of actual migration from predicted (%)')




## Cumsum -----------
plt_aggregated_cumsum = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(type ~ ., scales = 'free', ncol = 1) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_hi_cum * 100,
                  ymax = actual_cum / pred_lo_cum * 100,
                  fill = type),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = type)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = type),
             size = 2) +
  scale_color_manual(values = c(colorblind$lblue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$lblue,
                               colorblind$red)) +
  # scale_shape_manual(values = c(1, 21)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank()) +
  ylab('Cumsum')


