# options
options(width = 140)
Sys.setlocale("LC_TIME", "English")
knitr::opts_chunk$set(fig.width=13, fig.height=6) 

# colors 
colorblind = list(
  "blue"   = "#00798c",
  "red"    = "#d1495b",
  "yellow" = "#edae49",
  "green"  = "#66a182",
  "navy"   = "#2e4057",
  "grey"   = "#8d96a3"
)

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
               forecast,
               ggthemes)

