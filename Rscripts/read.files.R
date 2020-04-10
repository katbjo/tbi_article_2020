
#packages used
library("tidyverse")
library("lubridate")
library("ggplot2")

#read in all files together into one df
path <- "C:/tbi_paper/tbi_article_2020/Data/csv"
files  <- list.files(path, recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)

TBI_initial <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
str(TBI_initial)

empty_with <- 0.249    #weight of empty teabag (mean of 10 bags)
empty_without <- 0.154 #weight of empty teabag without label (mean of 10 bags)

#clean dataset
TBI_1  <- TBI_initial %>% 
  mutate(fLOC = factor(location), fSITE = factor(site), fHAB = factor(habitat), fYEAR = factor(year), fTTM = factor(ttm), fPLOT = factor(plot.code), fPAIR = factor(teabag.pair), fTYPE = factor(tea.type), fLENGHT = factor(length)) %>% 
  filter(!is.na(final.weight)) %>% 
  mutate(final.weight = as.numeric(final.weight)) %>% 
  mutate(INITIAL = initial.weight - empty_with) %>% #remove the teabag from the calculations
  mutate(FINAL = final.weight - empty_without) %>% #remove the teabag without label
  mutate(weight.loss = INITIAL - FINAL) %>% 
  mutate(weight.loss.0 = ifelse(weight.loss < 0, NA, weight.loss)) %>% #remove all negative values -> NA
  filter(!is.na(weight.loss.0)) %>% 
  mutate(percentage.remaining = round(FINAL/INITIAL*100, digits = 2)) %>%  # % of tea remaining
  mutate(percentage.loss = round(100 - percentage.remaining, digits = 2)) %>%  # % of tea lost
  mutate(decomp = (weight.loss.0/days.buried)*100) %>%  # biomass loss per day (milligrams)
  mutate(burial.date = lubridate::mdy(burial.date),
         recovery.date = lubridate::mdy(recovery.date)) #convert character to date format
  
str(TBI_1)


