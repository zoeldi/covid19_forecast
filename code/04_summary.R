
# Load ----------------------------------------------------------------------------------------

dat_base = readRDS(paste0(dir_data, '\\dat_base.RDS'))
dat_recon = readRDS(paste0(dir_data, '\\dat_recon.RDS'))

dat_base = 
  dat_base %>% 
  mutate(mod_family = case_when(.model_desc == 'ACTUAL' ~ 'Actual',
                                .model_desc == 'PROPHET' ~ 'Prophet',
                                .model_desc == 'PROPHET W XGBOOST ERRORS' ~ 'Prophet XGB',
                                .model_desc == 'TBATS   ' ~ 'TBATS'),
         actpred = case_when(.key == 'actual' ~ 'Actual',
                             .key == 'prediction' ~ 'Predicted'))

dat_recon = 
  dat_recon %>% 
  mutate(actpred = case_when(.key == 'actual' ~ 'Actual',
                             .key == 'prediction' ~ 'Predicted'))


plotlist = list()



# Base-level inflow by model ------------------------------------------------------------------

## Absolute -----------
(plt_base_inflow_abs =
   dat_base %>% 
   filter(type == 'Inflow') %>% 
   ggplot() +
   facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
   geom_ribbon(aes(x = migdate,
                   ymin = pred_lo,
                   ymax = pred_hi,
                   fill = mod_family),
               alpha = 0.2) +
   geom_line(aes(x = migdate,
                 y = pred_mi,
                 color = mod_family)) +
   geom_line(aes(x = migdate,
                 y = actual)) +
   scale_color_manual(values = c('black',
                                 colorblind$blue,
                                 colorblind$red,
                                 colorblind$yellow),
                      name = 'Model family') +
   scale_fill_manual(values = c('black',
                                colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
   theme_bw() + 
   theme(legend.position = 'bottom',
         axis.title.x = element_blank(),
         panel.grid.minor = element_blank(),
         text = element_text(size = 9)) +
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
  facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = mod_family),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = mod_family)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = mod_family),
             size = 1) +
   scale_color_manual(values = c(colorblind$blue,
                                 colorblind$red,
                                 colorblind$yellow),
                      name = 'Model family') +
   scale_fill_manual(values = c(colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual immigration from predicted (%)')
)




## Cumsum -----------
(plt_base_inflow_cumsum = 
   dat_base %>% 
   filter(type == 'Inflow' & migdate >= ymd('2020-01-01')) %>% 
   ggplot() +
   facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
   geom_hline(yintercept = 100,
              linetype = '31') +
   geom_ribbon(aes(x = migdate,
                   ymin = actual_cum / pred_lo_cum * 100,
                   ymax = actual_cum / pred_hi_cum * 100,
                   fill = mod_family),
               alpha = 0.2) +
   geom_line(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = mod_family)) +
   geom_point(aes(x = migdate,
                  y = actual_cum / pred_mi_cum * 100,
                  color = mod_family),
              size = 1) +
   scale_color_manual(values = c(colorblind$blue,
                                 colorblind$red,
                                 colorblind$yellow),
                      name = 'Model family') +
   scale_fill_manual(values = c(colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
   theme_bw() + 
   theme(legend.position = 'bottom',
         axis.title.x = element_blank(),
         panel.grid.minor = element_blank(),
         text = element_text(size = 9)) +
   ylab('Deviation of actual sum of immigrants from predicted (%)')
)





# Base-level outflow by model -----------------------------------------------------------------

## Absolute ---------------------
(plt_base_outflow_abs =
   dat_base %>% 
   filter(type == 'Outflow') %>% 
   ggplot() +
   facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
   geom_ribbon(aes(x = migdate,
                   ymin = pred_lo,
                   ymax = pred_hi,
                   fill = mod_family),
               alpha = 0.2) +
   geom_line(aes(x = migdate,
                 y = pred_mi,
                 color = mod_family)) +
   geom_line(aes(x = migdate,
                 y = actual)) +
   scale_color_manual(values = c('black',
                                 colorblind$blue,
                                 colorblind$red,
                                 colorblind$yellow),
                      name = 'Model family') +
   scale_fill_manual(values = c('black',
                                colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
   theme_bw() + 
   theme(legend.position = 'bottom',
         axis.title.x = element_blank(),
         panel.grid.minor = element_blank(),
         text = element_text(size = 9)) +
   ylab('Number of Hngarian emigrants')
)



## Percentage -----------
(plt_base_outflow_perc = 
  dat_base %>% 
  filter(type == 'Outflow' & migdate >= ymd('2020-01-01')) %>% 
  rowwise() %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100,
         issignificant = case_when(pred_hi_perc <= 100 & pred_lo_perc >= 100 ~ 'Non-Significant', TRUE ~ 'Significant')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_hi_perc,
                  ymax = pred_lo_perc,
                  fill = mod_family),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = pred_mi_perc,
                color = mod_family)) +
  geom_point(aes(x = migdate,
                 y = pred_mi_perc,
                 color = mod_family),
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red,
                               colorblind$yellow),
                    name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual emigration from predicted (%)'))




## Cumsum -----------
(plt_base_outflow_cumsum = 
  dat_base %>% 
  filter(type == 'Outflow' & migdate >= ymd('2020-01-01')) %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, agegrp, sep = ' | ') ~ ., scales = 'free', ncol = 4) +
  geom_hline(yintercept = 100,
             linetype = '31') +
  geom_ribbon(aes(x = migdate,
                  ymin = actual_cum / pred_hi_cum * 100,
                  ymax = actual_cum / pred_lo_cum * 100,
                  fill = mod_family),
              alpha = 0.2) +
  geom_line(aes(x = migdate,
                y = actual_cum / pred_mi_cum * 100,
                color = mod_family)) +
  geom_point(aes(x = migdate,
                 y = actual_cum / pred_mi_cum * 100,
                 color = mod_family),
             size = 1) +
   scale_color_manual(values = c(colorblind$blue,
                                 colorblind$red,
                                 colorblind$yellow),
                      name = 'Model family') +
   scale_fill_manual(values = c(colorblind$blue,
                                colorblind$red,
                                colorblind$yellow),
                     name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual sum of emigrants from predicted (%)'))




# Aggregated by age ---------------------------------------------------------------------------

## Absolute ---------------------
(plt_age_abs = 
  dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(agegrp, type, sep = ' | ') ~ ., ncol =2, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = actpred),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = actpred)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$blue)) +
  scale_fill_manual(values = c('black',
                               colorblind$blue)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Number of Hungarian migrants'))




## Percentage -----------
(plt_age_perc = 
  dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated' & migdate >= ymd('2020-01-01')) %>% 
  mutate(pred_lo_perc = (actual / pred_lo) * 100,
         pred_mi_perc = (actual / pred_mi) * 100,
         pred_hi_perc = (actual / pred_hi) * 100) %>% 
  ggplot() +
  facet_wrap(paste(agegrp, type, sep = ' | ') ~ ., scales = 'free', ncol = 2) +
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual migration from predicted (%)'))




## Cumsum -----------
(plt_age_cumsum = 
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual sum of migrants from predicted (%)'))





# Aggregated by host -------------------------------------------------------------------------

## Absolute ---------------------
(plt_host_abs = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp != 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(hostgrp, type, sep = ' | ') ~ ., ncol =2, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = actpred),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = actpred)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$blue)) +
  scale_fill_manual(values = c('black',
                               colorblind$blue)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Number of Hungarian migrants'))




## Percentage -----------
(plt_host_perc = 
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual migration from predicted (%)'))




## Cumsum -----------
(plt_host_cumsum = 
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual sum of migrants from predicted (%)'))





# Aggregated by type -------------------------------------------------------------------------

## Absolute ---------------------
(plt_aggregated_abs = 
  dat_recon %>% 
  filter(agegrp == 'Aggregated' & hostgrp == 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(type) ~ ., ncol = 1, scales = 'free_y') +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_lo,
                  ymax = pred_hi,
                  fill = actpred),
              alpha = 0.2) +
  geom_line(aes(x = migdate, 
                y = pred_mi,
                color = actpred)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$blue)) +
  scale_fill_manual(values = c('black',
                               colorblind$blue)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Number of Hungarian migrants'))



## Percentage -----------
(plt_aggregated_perc = 
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual migration from predicted (%)'))




## Cumsum -----------
(plt_aggregated_cumsum = 
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
             size = 1) +
  scale_color_manual(values = c(colorblind$blue,
                                colorblind$red)) +
  scale_fill_manual(values = c(colorblind$blue,
                               colorblind$red)) +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9)) +
  ylab('Deviation of actual sum of migrants from predicted (%)'))


# Save ----------------------------------------------------------------------------------------

for(i in ls()[substr(ls(), 1, 3) == 'plt']) {
  plotlist[[i]] = eval(as.name(i))
}

saveRDS(plotlist, paste0(dir_data, '\\plotlist.RDS'))

for(i in ls()[substr(ls(), 1, 3) == 'plt']) {
  ggsave(plot = eval(as.name(i)),
         filename = paste0(dir_out, '\\', i, '.jpeg'), width = 12)
}
