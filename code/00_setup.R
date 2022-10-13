# options
options(width = 140)
Sys.setlocale("LC_TIME", "English")
knitr::opts_chunk$set(fig.width=13, fig.height=6) 

# colors 
colorblind = list('grey' =   '#999999',
                  'orange' = '#E69F00',
                  'lblue' =  '#56B4E9',
                  'green' =  '#009E73',
                  'yellow' = '#F0E442',
                  'dblue' =  '#0072B2',
                  'red' =    '#D55E00',
                  'pink' =   '#CC79A7')

# path
dir_root = getwd()
dir_data = paste0(dir_root, '\\data')
dir_code = paste0(dir_root, '\\code')
dir_out = paste0(dir_root, '\\out')

# libs
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               tidyquant,
               tidymodels,
               timetk,
               tsibble,
               fabletools,
               feather,
               modeltime,
               modeltime.ensemble,
               modeltime.resample,
               ggpubr,
               forecast)

