
Full_data <- TBI_1 %>% 
  filter(fYEAR != "2018", fTTM != "grazed", fTYPE == "G", fHAB != "NNW moss", plot_code != "CL11",
         plot_code != "CL12", plot_code != "CL13", plot_code != "CL14", plot_code != "CL15",
         plot_code != "CL16", plot_code != "CL17", plot_code != "CL18", plot_code != "CL19",
         plot_code != "CL20", plot_code != "DL11", plot_code != "DL12", plot_code != "DL13",
         plot_code != "DL14", plot_code != "DL15", plot_code != "DL16", plot_code != "DL17",
         plot_code != "DL18", plot_code != "DL19", plot_code != "DL20")

#### Is the mass loss of 3 months correlated with mass loss of 12 months??

Corr_3vs12 <- Full_data %>% 
  filter(fYEAR == "2016") %>%
  filter(!is.na(decomp)) %>% 
  select(fLENGHT, decomp) %>% 
  group_by(fLENGHT) %>%
  dplyr::mutate(i1 = row_number()) %>% 
  spread(fLENGHT, decomp) %>% 
  rename(full_year = "12 months") %>% 
  rename(three_months = "3 months") %>% 
  filter(!is.na(full_year))

cor(Corr_3vs12$full_year, Corr_3vs12$three_months, 
    method=c("pearson", "kendall", "spearman"))

library("ggpubr")
ggscatter(Corr_3vs12, x = "full_year", y = "three_months", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


### There is not a strong correlation - R = 0.34, p = 0.0021

dat_2016 <- Full_data %>% 
  filter(fLENGHT == "12 months", fYEAR == "2016") %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, weight_loss_0, 
         percentage_loss, decomp, soil_moisture_m) %>% 
  filter(!is.na(soil_moisture_m))

dat_2017 <- Full_data %>% 
  filter(fLENGHT == "12 months", fYEAR == "2017") %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, weight_loss_0, 
         percentage_loss, decomp, soil_moisture_m)

### Relationship between decomp and soil moisture

cor(dat_2016$decomp, dat_2016$soil_moisture_m, 
    method=c("pearson", "kendall", "spearman"))

ggscatter(dat_2016, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2016_dry <- dat_2016 %>% 
  filter(fHAB == "Dryas")
ggscatter(dat_2016_dry, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2017_dry <- dat_2017 %>% 
  filter(fHAB == "Dryas")
ggscatter(dat_2017_dry, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2016_cas <- dat_2016 %>% 
  filter(fHAB == "Cassiope")
ggscatter(dat_2016_cas, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2017_cas <- dat_2017 %>% 
  filter(fHAB == "Cassiope")
ggscatter(dat_2017_cas, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2016_snow <- dat_2016 %>% 
  filter(fHAB == "Snowbed")
ggscatter(dat_2016_snow, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2017_snow <- dat_2017 %>% 
  filter(fHAB == "Snowbed")
ggscatter(dat_2017_snow, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2016_aud <- dat_2016 %>% 
  filter(fHAB == "shrub heath")
ggscatter(dat_2016_aud, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2017_aud <- dat_2017 %>% 
  filter(fHAB == "shrub heath")
ggscatter(dat_2017_aud, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

dat_2016_thing <- dat_2016 %>% 
  filter(fHAB == "moss heath")
ggscatter(dat_2016_thing, x = "decomp", y = "soil_moisture_m", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

## Only a strong relationship in the snowbed 2016 !!!!

TBI_dat <- Full_data %>% 
  filter(fLENGHT == "12 months") %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, weight_loss_0, 
         percentage_loss, decomp, soil_moisture_m)

