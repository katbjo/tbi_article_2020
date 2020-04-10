TBI_3months <- TBI_1 %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, weight.loss.0, percentage.loss, decomp, soil.moisture.m) %>% 
  filter(fLENGHT == "3", fHAB != "NNW moss", fYEAR != "2018", !fTTM %in% c("grazed", "exclosure"), !fPLOT %in% c("DL11", "DL12", "DL13", "DL14", "DL15",
                                                                                                                 "DL16", "DL17", "DL18", "DL19", "DL20",
                                                                                                                 "CL11", "CL12", "CL13", "CL14", "CL15",
                                                                                                                 "CL16", "CL17", "CL18", "CL19", "CL20"))

TBI_12months <- TBI_1 %>% 
  select(fLOC, fSITE, fHAB, fYEAR, fLENGHT, fTTM, fPLOT, fPAIR, fTYPE, weight.loss.0, percentage.loss, decomp, soil.moisture.m) %>% 
  filter(fLENGHT == "12", fHAB != "NNW moss", fYEAR != "2018", !fTTM %in% c("grazed", "exclosure"), !fPLOT %in% c("DL11", "DL12", "DL13", "DL14", "DL15",
                                                                                                                 "DL16", "DL17", "DL18", "DL19", "DL20",
                                                                                                                 "CL11", "CL12", "CL13", "CL14", "CL15",
                                                                                                                 "CL16", "CL17", "CL18", "CL19", "CL20"))

str(TBI_12months)

aud_green <- TBI_2 %>% 
  filter(fSITE == "Audkuluheidi", fTTM != "grazed", fTYPE == "G") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)




aud_green_P1 <- ggplot(aud_green, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#719803") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("Auðkúluheiði") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

aud_green_P1

thing_green <- TBI_2 %>% 
  filter(fSITE == "Thingvellir", fTTM != "exclosure", fTYPE == "G") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

thing_green_P1 <- ggplot(thing_green, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#719803") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("Thingvellir") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

thing_green_P1


dry_green <- TBI_2 %>% 
  filter(fHAB == "Dryas", fTTM != "exclosure", fTYPE == "G") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

dryas <- expression(paste(italic("Dryas"), " heath"))
dry_green_P1 <- ggplot(dry_green, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#719803") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle(dryas) +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

dry_green_P1

cas_green <- TBI_2 %>% 
  filter(fHAB == "Cassiope", fTTM != "exclosure", fTYPE == "G") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

cassiope <- expression(paste(italic("Cassiope"), " heath"))
cas_green_P1 <- ggplot(cas_green, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#719803") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle(cassiope) +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

cas_green_P1

snow_green <- TBI_2 %>% 
  filter(fHAB == "Snowbed", fTTM != "exclosure", fTYPE == "G") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

snow_green_P1 <- ggplot(snow_green, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#719803") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("Snowbed") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

snow_green_P1

snow_3 <- snow_green %>% 
  filter(fLENGHT == "3 months")

dry_3 <- dry_green %>% 
  filter(fLENGHT == "3 months")

cas_3 <- cas_green %>% 
  filter(fLENGHT == "3 months")

#### RED TEA ###

aud_red <- TBI_2 %>% 
  filter(fSITE == "Audkuluheidi", fTTM != "grazed", fTYPE == "R") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)




aud_red_P1 <- ggplot(aud_red, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#AC3D01") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("AuÃ°kÃºluheiÃ°i") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

aud_red_P1

thing_red <- TBI_2 %>% 
  filter(fSITE == "Thingvellir", fTTM != "exclosure", fTYPE == "R") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

thing_red_P1 <- ggplot(thing_red, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#AC3D01") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("Ãžingvellir") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

thing_red_P1


dry_red <- TBI_2 %>% 
  filter(fHAB == "Dryas", fTTM != "exclosure", fTYPE == "R") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

dryas <- expression(paste(italic("Dryas"), " heath"))
dry_red_P1 <- ggplot(dry_red, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#AC3D01") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle(dryas) +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

dry_red_P1

cas_red <- TBI_2 %>% 
  filter(fHAB == "Cassiope", fTTM != "exclosure", fTYPE == "R") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

cassiope <- expression(paste(italic("Cassiope"), " heath"))
cas_red_P1 <- ggplot(cas_red, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#AC3D01") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle(cassiope) +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

cas_red_P1

snow_red <- TBI_2 %>% 
  filter(fHAB == "Snowbed", fTTM != "exclosure", fTYPE == "R") %>% 
  group_by(fYEAR, fLENGHT, fTTM) %>% 
  summarise(mean = mean(percentage.loss), sd = sd(percentage.loss), n = n()) %>% 
  mutate(se = sd / sqrt(n),
         ci = qnorm(0.975)* se) %>% 
  gather(key = key, value = mean, - fYEAR, - fLENGHT, - fTTM, - sd, - se, - n, - ci)

snow_red_P1 <- ggplot(snow_red, aes(x = fYEAR, y = mean, shape = factor(fTTM))) +
  theme_bw() +
  facet_grid(~ fct_rev(fLENGHT), scales = "free") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), size = 0.5, width
                = 0.2, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), size = 2.0, width
                = 0, position = position_dodge(0.5)) +
  geom_point(size = 10, stat = "identity", position = position_dodge(width = 0.5), fill = "#AC3D01") +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "Year", y = "Mass loss (%)") +
  ggtitle("Snowbed") +
  theme(plot.title = element_text(size = 30,hjust = 0.5), legend.title = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 18),
        strip.background = element_rect(fill = "#F8F9F9", color = "black"))

snow_red_P1
