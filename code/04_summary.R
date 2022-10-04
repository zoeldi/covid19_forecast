
# Base-level inflow by model ------------------------------------------------------------------

dat_base %>% 
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
                                colorblind$red),
                     name = 'Model family') +
  scale_fill_manual(values = c('black',
                               colorblind$green,
                               colorblind$lblue,
                               colorblind$red),
                    name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Number of Hngarian immigrants')

ggsave(paste0(dir_out, '\\plt_base_inflow.png'), width = 40, height = 20, units = "cm")
  

# Base-level outflow by model -----------------------------------------------------------------

dat_base %>% 
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
                                colorblind$red),
                     name = 'Model family') +
  scale_fill_manual(values = c('black',
                               colorblind$green,
                               colorblind$lblue,
                               colorblind$red),
                    name = 'Model family') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        axis.title.x = element_blank()) +
  ylab('Number of Hngarian emigrants')

ggsave(paste0(dir_out, '\\plt_base_outflow.png'), width = 40, height = 20, units = "cm")


# Aggregated by age ---------------------------------------------------------------------------

dat_recon %>% 
  filter(agegrp != 'Aggregated' & hostgrp == 'Aggregated') %>% 
  ggplot() +
  facet_wrap(paste(type, agegrp, sep = ' | ') ~ ., ncol =3, scales = 'free_y') +
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
                                colorblind$red),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$red),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')

ggsave(paste0(dir_out, '\\plt_age_inflow.png'), width = 40, height = 20, units = "cm")


# Aggregated by host -------------------------------------------------------------------------

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
                color = .key)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$red),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$red),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')

ggsave(paste0(dir_out, '\\plt_host_inflow.png'), width = 40, height = 20, units = "cm")


# Aggregated by type -------------------------------------------------------------------------

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
                color = .key)) +
  geom_line(aes(x = migdate,
                y = actual)) +
  scale_color_manual(values = c('black',
                                colorblind$red),
                     name = '') +
  scale_fill_manual(values = c('black',
                               colorblind$red),
                    name = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab('Number of Hungarian migrants')

ggsave(paste0(dir_out, '\\plt_aggregated.png'), width = 40, height = 20, units = "cm")
