# Zillow API
# devtools::install_github("xiyuansun/realEstAnalytics")
library(realEstAnalytics)
library(tidyverse)
library(config) # install.packages("config")

# Zillow API Key from https://www.zillow.com/howto/api/APIOverview.htm
set_zillow_web_service_id('X1-ZWz17cgknxh91n_85kuc')

config::get("zillow", file="../config.yml") %>% 
  set_zillow_web_service_id()
