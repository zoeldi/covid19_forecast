dat_o_v1 = readRDS(paste0(dir_data, '\\dat_o_v1.RData'))
dat_i_v1 = readRDS(paste0(dir_data, '\\dat_i_v1.RData'))

# Bind inflow and outflow
dat_final_v1 = 
  bind_rows(dat_i_v1 %>% mutate(flow = 'Inflow'),
            dat_o_v1 %>% mutate(flow = 'Outflow')) %>% 
  mutate(agegrp = case_when(is.na(agegrp) ~ 'Aggregated',
                            TRUE ~ agegrp),
         hostgrp = case_when(is.na(hostgrp) ~ 'Aggregated',
                             TRUE ~ hostgrp))


# Create measures
dat_final_v1 = 
  dat_final_v1 %>% 
  group_by(flow, agegrp, hostgrp) %>% 
  arrange(flow, agegrp, hostgrp, migdate) %>% 
  mutate(act_sum = cumsum(act_abs_mth),
         pred_sum = cumsum(pred_v1),
         pred_lo_sum = cumsum(case_when(is.na(pred_v1_lo) ~ pred_v1,
                                        TRUE ~ pred_v1_lo)),
         pred_hi_sum = cumsum(case_when(is.na(pred_v1_hi) ~ pred_v1,
                                        TRUE ~ pred_v1_hi)),
         effect_sum_perc = pred_sum / act_sum - 1,
         effect_lo_sum_perc = pred_lo_sum / act_sum - 1,
         effect_hi_sum_perc = pred_hi_sum / act_sum - 1,
         effect_perc = act_abs_mth / pred_v1 - 1,
         effect_lo_perc = act_abs_mth / pred_v1_lo - 1,
         effect_hi_perc = act_abs_mth / pred_v1_hi - 1,
  )

# TS-Comapre (abs) ----------------------------------------------------------------------------------

## Low-level inflow ---------------------------------------------------------------------------

# AUT
plt_abs_inflow_aut_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'AUT' 
         #& migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_aut_low

ggsave(plt_abs_inflow_aut_low, filename = paste0(dir_out, '\\plt_abs_inflow_aut_low.png'))

# BENELUX
plt_abs_inflow_ben_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'BENELUX/NORDIC' 
         #& migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_ben_low

ggsave(plt_abs_inflow_ben_low, filename = paste0(dir_out, '\\plt_abs_inflow_ben_low.png'))


# DEU
plt_abs_inflow_deu_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'DEU' 
         #& migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_deu_low

ggsave(plt_abs_inflow_deu_low, filename = paste0(dir_out, '\\plt_abs_inflow_deu_low.png'))


# GBR/IRL
plt_abs_inflow_grb_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'GRB/IRL' 
         #& migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_grb_low

ggsave(plt_abs_inflow_grb_low, filename = paste0(dir_out, '\\plt_abs_inflow_grb_low.png'))

# OTH
plt_abs_inflow_oth_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'OTH' 
         & migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_oth_low

ggsave(plt_abs_inflow_oth_low, filename = paste0(dir_out, '\\plt_abs_inflow_oth_low.png'))


## Low-level outflow --------------------------------------------------------------------------

# AUT
plt_abs_outflow_aut_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'AUT' 
         #& migdate >= '2015-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_outflow_aut_low

ggsave(plt_abs_outflow_aut_low, filename = paste0(dir_out, '\\plt_abs_outflow_aut_low.png'))

# BENELUX
plt_abs_outflow_ben_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'BENELUX/NORDIC' 
         #& migdate >= '2015-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_outflow_ben_low

ggsave(plt_abs_outflow_ben_low, filename = paste0(dir_out, '\\plt_abs_outflow_ben_low.png'))


# DEU
plt_abs_outflow_deu_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'DEU' 
         #& migdate >= '2015-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_outflow_deu_low

ggsave(plt_abs_outflow_deu_low, filename = paste0(dir_out, '\\plt_abs_outflow_deu_low.png'))


# GBR/IRL
plt_abs_outflow_grb_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'GRB/IRL' 
         #& migdate >= '2015-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_outflow_grb_low

ggsave(plt_abs_outflow_grb_low, filename = paste0(dir_out, '\\plt_abs_outflow_grb_low.png'))

# OTH
plt_abs_outflow_oth_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'OTH' 
         #& migdate >= '2015-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_outflow_oth_low

ggsave(plt_abs_outflow_oth_low, filename = paste0(dir_out, '\\plt_abs_outflow_oth_low.png'))

## Mid-level intflow -----------------------------------------------------------------

# Age
plt_abs_inflow_age_mid = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'Aggregated' 
         #& migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ ., scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_age_mid

ggsave(plt_abs_inflow_age_mid, filename = paste0(dir_out, '\\plt_abs_inflow_age_mid.png'))

# Host
plt_abs_inflow_host_mid = 
  dat_final_v1 %>%  
  filter(agegrp == 'Aggregated' 
         & hostgrp != 'Aggregated' 
        # & migdate >= '2015-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(hostgrp ~ ., scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_inflow_host_mid

ggsave(plt_abs_inflow_host_mid, filename = paste0(dir_out, '\\plt_abs_inflow_host_mid.png'))

## High-level  ----------------------------------------------------------------------

plt_abs_high = 
  dat_final_v1 %>%  
  filter(agegrp == 'Aggregated' 
         & hostgrp == 'Aggregated') %>%  
        # & migdate >= '2015-01-01') %>% 
  ggplot() +
  facet_grid(flow ~ ., scales = 'free') +
  geom_line(aes(x = migdate,
                y = pred_v1,
                color = 'Predicted'),
            linetype = '31') +
  geom_line(aes(x = migdate,
                y = act_abs_mth,
                color = 'Actual')) +
  geom_ribbon(aes(x = migdate,
                  ymin = pred_v1_lo,
                  ymax = pred_v1_hi),
              alpha = 0.2,
              fill = colorblind$red) +
  scale_color_manual(values = c('black', colorblind$red)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_abs_high

ggsave(plt_abs_high, filename = paste0(dir_out, '\\plt_abs_high.png'))


# TS-Comapre (perc) ----------------------------------------------------------------------------------

## Low-level inflow ---------------------------------------------------------------------------

# AUT
plt_perc_inflow_aut_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'AUT' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_aut_low

ggsave(plt_perc_inflow_aut_low, filename = paste0(dir_out, '\\plt_perc_inflow_aut_low.png'))

# BENELUX
plt_perc_inflow_ben_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'BENELUX/NORDIC' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_ben_low

ggsave(plt_perc_inflow_ben_low, filename = paste0(dir_out, '\\plt_perc_inflow_ben_low.png'))


# DEU
plt_perc_inflow_deu_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'DEU' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_deu_low

ggsave(plt_perc_inflow_deu_low, filename = paste0(dir_out, '\\plt_perc_inflow_deu_low.png'))


# GBR/IRL
plt_perc_inflow_grb_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'GRB/IRL' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_grb_low

ggsave(plt_perc_inflow_grb_low, filename = paste0(dir_out, '\\plt_perc_inflow_grb_low.png'))

# OTH
plt_perc_inflow_oth_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'OTH' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_oth_low

ggsave(plt_perc_inflow_oth_low, filename = paste0(dir_out, '\\plt_perc_inflow_oth_low.png'))


## Low-level outflow --------------------------------------------------------------------------

# AUT
plt_perc_outflow_aut_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'AUT' 
         & migdate >= '2020-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_outflow_aut_low

ggsave(plt_perc_outflow_aut_low, filename = paste0(dir_out, '\\plt_perc_outflow_aut_low.png'))

# BENELUX
plt_perc_outflow_ben_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'BENELUX/NORDIC' 
         & migdate >= '2020-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_outflow_ben_low

ggsave(plt_perc_outflow_ben_low, filename = paste0(dir_out, '\\plt_perc_outflow_ben_low.png'))


# DEU
plt_perc_outflow_deu_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'DEU' 
         & migdate >= '2020-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_outflow_deu_low

ggsave(plt_perc_outflow_deu_low, filename = paste0(dir_out, '\\plt_perc_outflow_deu_low.png'))


# GBR/IRL
plt_perc_outflow_grb_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'GRB/IRL' 
         & migdate >= '2020-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_outflow_grb_low

ggsave(plt_perc_outflow_grb_low, filename = paste0(dir_out, '\\plt_perc_outflow_grb_low.png'))

# OTH
plt_perc_outflow_oth_low = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'OTH' 
         & migdate >= '2020-01-01'
         & flow == 'Outflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ hostgrp, scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_outflow_oth_low

ggsave(plt_perc_outflow_oth_low, filename = paste0(dir_out, '\\plt_perc_outflow_oth_low.png'))

## Mid-level intflow -----------------------------------------------------------------

# Age
plt_perc_inflow_age_mid = 
  dat_final_v1 %>%  
  filter(agegrp != 'Aggregated' 
         & hostgrp == 'Aggregated' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(agegrp ~ ., scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_age_mid

ggsave(plt_perc_inflow_age_mid, filename = paste0(dir_out, '\\plt_perc_inflow_age_mid.png'))

# Host
plt_perc_inflow_host_mid = 
  dat_final_v1 %>%  
  filter(agegrp == 'Aggregated' 
         & hostgrp != 'Aggregated' 
         & migdate >= '2020-01-01'
         & flow == 'Inflow') %>% 
  ggplot() +
  facet_grid(hostgrp ~ ., scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_inflow_host_mid

ggsave(plt_perc_inflow_host_mid, filename = paste0(dir_out, '\\plt_perc_inflow_host_mid.png'))

## High-level  ----------------------------------------------------------------------

plt_perc_high = 
  dat_final_v1 %>%  
  filter(agegrp == 'Aggregated' 
         & hostgrp == 'Aggregated' 
         & migdate >= '2020-01-01') %>% 
  ggplot() +
  facet_grid(flow ~ ., scales = 'free') +
  geom_hline(yintercept = 0,
             linetype = '31') +
  geom_line(aes(x = migdate,
                y = effect_perc)) +
  geom_point(aes(x = migdate,
                 y = effect_perc)) +
  geom_ribbon(aes(x = migdate,
                  ymin = effect_lo_perc,
                  ymax = effect_hi_perc),
              alpha = 0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'top',
        text = element_text(size = 15),
        legend.key.width = unit(1,"cm")) +
  ylab('Actual / Predicted') +
  guides(color = guide_legend(override.aes = list(size = 2))); plt_perc_high

ggsave(plt_perc_high, filename = paste0(dir_out, '\\plt_perc_high.png'))

