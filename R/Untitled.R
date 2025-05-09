library(tidyverse)
load('KN_Recent_Nebel_Pro_Tag.RData')
recent = nebel_pro_tag
load('KN_Historisch_Nebel_Pro_Tag.RData')
hist = nebel_pro_tag
nebel_pro_tag = rbind(hist, recent)


