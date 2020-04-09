
library("ggpubr")

TBI_reg <- TBI_1 %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, percentage_loss, decomp, moss_thickness, soil_depth, soil_moisture_m) %>% 
  mutate(soil_depth = as.numeric(soil_depth)) %>% 
  mutate(moss_thickness = as.numeric(moss_thickness))

aud_reg_green <- TBI_reg %>% 
  filter(fSITE == "Audkuluheidi", fTTM != "grazed", fTYPE == "G", fYEAR == "2016")


cor(aud_reg_green$percentage_loss, aud_reg_green$moss_thickness, 
    method=c("pearson", "kendall", "spearman"))

ggscatter(aud_reg_green, x = "percentage_loss", y = "moss_thickness", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass loss of green tea (%)", ylab = "Moss thickness")

snow_reg_green <- TBI_reg %>% 
  filter(fHAB == "Snowbed", fTTM != "grazed", fTYPE == "G", fYEAR == "2016")


cor(snow_reg_green$percentage_loss, snow_reg_green$soil_depth, 
    method=c("pearson", "kendall", "spearman"))

ggscatter(snow_reg_green, x = "percentage_loss", y = "soil_depth", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass loss of green tea (%)", ylab = "Moss thickness")
