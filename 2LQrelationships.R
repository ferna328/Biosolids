######################## 2LQrelationships.R ####################################
# Research Paper - A Simple Framework for Attibution of Changes in             # 
#                  Concentration-Load-Discharge Relationships and Estimating   #
#                  the Longevity of Biosolids Legacy Phosphorus                #
#                                                                              #
# Funding - National Science Foundation: Award 2129926                         #
#           United States Department of Agriculture, Natural Resources         #
#                  Conservation Service: Award NR233A750023C002                #
#           St. Johns River Water Management District: project MN017           #
#                                                                              #
# Coded by - Nicolas Fernandez (OrcID 0000-0001-7979-2941)                     #
#            Soil Water and Ecosystem Sciences Department                      #
#            University of Florida, Gainesville, FL, United States             #
#                                                                              #
# Paper Authors - Nicolas Fernandez (OrcID 0000-0001-7979-2941)                #
#                 Jaehyeon Lee (OrcID 0000-0002-2643-4582)                     #
#                 James W. Jawitz (OrcID 0000-0002-6745-0765)                  #
#               Soil Water and Ecosystem Sciences Department                   #
#               University of Florida, Gainesville, FL, United States          #
######################## Description ###########################################
# This code is written to show the linear regressions explained in in the      #
# paper and presented in Fig. 3                                                #
################################################################################
###### 0. Set working Directory and load libraries and data ####################
# 0.1 Set working directory ____________________________________________________
wd = paste("C:/XXX/YYY/ZZZ", sep = "/")
setwd(wd)
# 0.2 Load libraries ___________________________________________________________
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(prophet)
library(patchwork)
# 0.3 Load data ________________________________________________________________
load(
  file = 
    "C:/XXX/YYY/ZZZ/1BreakpointsResults.RData")
###### 1. Make panels for Figure 3 #############################################
# a) Fort Drum _________________________________________________________________
Color1 = rgb(173/256, 216/256, 230/256) # light blue
Color2 = rgb(0,0,0.8)
ggplot(LAmbientCreeksNoOut[[1]], aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  scale_x_log10(limits = c(0.0000001, 0.01)) +
  scale_y_log10() +
  geom_smooth(formula = 'y ~ x', 
              method = "lm", 
              se = FALSE, color = Color2) + 
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL, 
       y = expression(TP~"[ g m"^-2~"day"^-1~"]"), 
       title = "Fort Drum (FDC)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 200 x 160
# b) Blue Cypress ______________________________________________________________
Color1 = rgb(173/256, 216/256, 230/256) # light blue
Color2 = rgb(0,0,0.8)
Color3 = rgb(250/256, 160/256, 160/256) # light red
Color4 = rgb(0.8,0,0)
ggplot(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[3]] |> 
               filter(datetime > BreakPoints[[3]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.000001, 0.0001, 0.01),
                limits = c(0.0000006, 0.012)) +
  scale_y_log10(breaks = c(0.0000001, 0.00001, 0.001),
                limits = c(0.0000001, 0.003)) +
  geom_smooth(data = LAmbientCreeksNoOut[[3]] |> 
                filter(datetime <= BreakPoints[[3]]),
    formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[3]] |> 
                filter(datetime > BreakPoints[[3]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL , 
       y = NULL, 
       title = "Blue Cypress (BCC)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 180 x 160
# c) South Wolf ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[8]] |> 
               filter(datetime > BreakPoints[[8]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.00001, 0.001),
                limits = c(0.0000028, 0.014)) +
  scale_y_log10(breaks = c(0.00001, 0.001),
                limits = c(0.00000031, 0.003)) +
  geom_smooth(data = LAmbientCreeksNoOut[[8]] |> 
                filter(datetime <= BreakPoints[[8]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[8]] |> 
                filter(datetime > BreakPoints[[8]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL , 
       y = NULL, 
       title = "South Wolf (SWOLFU)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 180 x 160
# d) Six Mile ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[4]] |> 
               filter(datetime > BreakPoints[[4]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.000001, 0.0001, 0.01),
                limits = c(0.0000004, 0.011)) +
  scale_y_log10(breaks = c(0.0000001, 0.00001, 0.001),
                limits = c(0.00000002, 0.005)) +
  geom_smooth(data = LAmbientCreeksNoOut[[4]] |> 
                filter(datetime <= BreakPoints[[4]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2,
              linetype = "solid",
              linewidth = 1.5) + 
  geom_smooth(data = LAmbientCreeksNoOut[[4]] |> 
                filter(datetime > BreakPoints[[4]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4,
              linetype = "solid",
              linewidth = 0.71) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL, 
       y = expression(TP~"[ g m"^-2~"day"^-1~"]"), 
       title = "Six Mile (SCR)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 200 x 160
# e) Pennywash ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[5]] |> 
               filter(datetime > BreakPoints[[5]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.000001, 0.0001, 0.01),
                limits = c(0.0000005, 0.02)) +
  scale_y_log10(breaks = c(0.0000001, 0.00001, 0.001),
                limits = c(0.000000024, 0.005)) +
  geom_smooth(data = LAmbientCreeksNoOut[[5]] |> 
                filter(datetime <= BreakPoints[[5]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[5]] |> 
                filter(datetime > BreakPoints[[5]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL , 
       y = NULL, 
       title = "Pennywash (PENNY)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 180 x 160
# f) Jane Green ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[7]] |> 
               filter(datetime > BreakPoints[[7]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.0000001, 0.00001, 0.001),
                limits = c(0.00000003, 0.02)) +
  scale_y_log10(breaks = c(0.0000001, 0.00001, 0.001),
                limits = c(0.000000002, 0.02)) +
  geom_smooth(data = LAmbientCreeksNoOut[[7]] |> 
                filter(datetime <= BreakPoints[[7]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) +
  geom_smooth(data = LAmbientCreeksNoOut[[7]] |> 
                filter(datetime > BreakPoints[[7]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL , 
       y = NULL, 
       title = "Jane Green (JGS)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 180 x 160
# g) Crabgrass ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[9]] |> 
               filter(datetime > BreakPoints[[9]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.000001, 0.0001, 0.01),
                limits = c(0.0000005, 0.02)) +
  scale_y_log10(breaks = c(0.0000001, 0.00001, 0.001),
    limits = c(0.00000005, 0.005)) +
  geom_smooth(data = LAmbientCreeksNoOut[[9]] |> 
                filter(datetime <= BreakPoints[[9]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[9]] |> 
                filter(datetime > BreakPoints[[9]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = expression(Q ~"[ m day"^-1~"]") , 
       y = expression(TP~"[ g m"^-2~"day"^-1~"]"), 
       title = "Crabgrass (USJ055)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# 200*180
# h) Tenmile ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[2]] |> 
               filter(datetime > BreakPoints[[2]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.0001, 0.01),
                limits = c(0.000007, 0.02)) +
  scale_y_log10(breaks = c(0.000001, 0.0001, 0.01),
                limits = c(0.0000005, 0.03)) +
  geom_smooth(data = LAmbientCreeksNoOut[[2]] |> 
                filter(datetime <= BreakPoints[[2]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[2]] |> 
                filter(datetime > BreakPoints[[2]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = NULL, #expression(Q ~"[ m day"^-1~"]") , 
       y = NULL, 
       title = NULL #"Tenmile (TMC)"
       ) +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
#180 * 180
# i) Taylor ______________________________________________________________
ggplot(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]), 
       aes(x = Qmday, y = `loadTP-TperA`)) +
  geom_point(color = Color1) +
  geom_point(data = LAmbientCreeksNoOut[[6]] |> 
               filter(datetime > BreakPoints[[6]]),
             aes(x = Qmday, y = `loadTP-TperA`),
             color = Color3) +
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01),
                limits = c(0.00008, 0.01)) +
  scale_y_log10(breaks = c(0.00001, 0.0001, 0.001),
                limits = c(0.000004, 0.002)) +
  geom_smooth(data = LAmbientCreeksNoOut[[6]] |> 
                filter(datetime <= BreakPoints[[6]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color2) + 
  geom_smooth(data = LAmbientCreeksNoOut[[6]] |> 
                filter(datetime > BreakPoints[[6]]),
              formula = 'y ~ x', method = "lm", 
              se = FALSE, color = Color4) +
  theme_minimal() +  # Use a minimalistic theme
  labs(x = expression(Q ~"[ m day"^-1~"]") , 
       y = NULL, 
       title = "Taylor (TCR)") +
  theme(legend.position = "none",
        axis.title = element_text(size = 5.8),       # Remove axis titles
        axis.text = element_text(size = 4.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, hjust = 0.1, vjust = 0.80))
# export as 180 x 180
###### 2. Get values of LQ relationships #######################################
LAmbientCreeksNoOut <- LAmbientCreeksNoOut |> 
  map(function(creek){
    creek |> 
      mutate(loadgm2day = `loadTP-TperA`, # load in g per m2 per day
             x = log10(Qmday), # log transform
             y = log10(loadgm2day))})
LAmbientCreeksNoOut <- LAmbientCreeksNoOut |> 
  map(function(creek){
    creek |> 
      mutate(loadgm2day = `loadTP-TperA`, # load in g per m2 per day
             xln = log(Qmday), # log transform
             yln = log(loadgm2day))})
# a) Fort Drum _________________________________________________________________
model1 <- lm(y ~ x, data = LAmbientCreeksNoOut[[1]])
model1$coefficients
10^(model1$coefficients[[1]]) # 0.2000315 g/m3 or mg/l
10^(confint(model1)[[1]]) # 0.1486315
10^(confint(model1)[[3]]) # 0.2692068
model1$coefficients[[2]] - 1 # 0.008664645 b units
confint(model1)[[2]] - 1 # -0.02328325
confint(model1)[[4]] - 1 # 0.04061254
summary(model1)$r.squared # 0.9578294

model1ln <- lm(yln ~ xln, data = LAmbientCreeksNoOut[[1]])
model1ln$coefficients
exp(model1ln$coefficients[[1]]) # 0.2000315 g/m3 or mg/l
exp(confint(model1ln)[[1]]) # 0.1486315
exp(confint(model1ln)[[3]]) # 0.2692068
model1ln$coefficients[[2]] - 1 # 0.008664645 b units
confint(model1ln)[[2]] - 1 # -0.02328325
confint(model1ln)[[4]] - 1 # 0.04061254
summary(model1ln)$r.squared # 0.9578294
# b) Blue Cypress ______________________________________________________________
# Before break -----------------------------------------------------------------
model21 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[3]] |> 
                filter(datetime <= BreakPoints[[3]]))
model21$coefficients
10^(model21$coefficients[[1]]) # 0.2399886 g/m3 or mg/l
10^(confint(model21)[[1]]) # 0.1636453
10^(confint(model21)[[3]]) # 0.3519474
model21$coefficients[[2]] - 1 # 0.06297479 b units
confint(model21)[[2]] - 1 # 0.01835453
confint(model21)[[4]] - 1 # 0.107595
summary(model21)$r.squared # 0.949235

model21ln <- lm(yln ~ xln, 
              data = LAmbientCreeksNoOut[[3]] |> 
                filter(datetime <= BreakPoints[[3]]))
model21ln$coefficients
exp(model21ln$coefficients[[1]]) # 0.2399886 g/m3 or mg/l
exp(confint(model21ln)[[1]]) # 0.1636453
exp(confint(model21ln)[[3]]) # 0.3519474
model21ln$coefficients[[2]] - 1 # 0.06297479 b units
confint(model21ln)[[2]] - 1 # 0.01835453
confint(model21ln)[[4]] - 1 # 0.107595
summary(model21ln)$r.squared # 0.949235
# After break ------------------------------------------------------------------
model22 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[3]] |> 
                filter(datetime > BreakPoints[[3]]))
model22$coefficients
10^(model22$coefficients[[1]]) # 0.2375363 g/m3 or mg/l
10^(confint(model22)[[1]]) # 0.1912227
10^(confint(model22)[[3]]) # 0.2950669
model22$coefficients[[2]] - 1 # 0.04195223 b units
confint(model22)[[2]] - 1 # 0.0166896
confint(model22)[[4]] - 1 # 0.06721486
summary(model22)$r.squared # 0.9735434
# c) South Wolf ________________________________________________________________
# Before break -----------------------------------------------------------------
model31 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[8]] |> 
                filter(datetime <= BreakPoints[[8]]))
model31$coefficients
10^(model31$coefficients[[1]]) # 0.3280198 g/m3 or mg/l
10^(confint(model31)[[1]]) # 0.1982402
10^(confint(model31)[[3]]) # 0.5427609
model31$coefficients[[2]] - 1 # 0.1211382 b units
confint(model31)[[2]] - 1 # 0.05260194
confint(model31)[[4]] - 1 # 0.1896745
summary(model31)$r.squared # 0.881196
# After break ------------------------------------------------------------------
model32 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[8]] |> 
                filter(datetime > BreakPoints[[8]]))
model32$coefficients
10^(model32$coefficients[[1]]) # 0.3280198 g/m3 or mg/l
10^(confint(model32)[[1]]) # 0.1982402
10^(confint(model32)[[3]]) # 0.5427609
model32$coefficients[[2]] - 1 # 0.1211382 b units
confint(model32)[[2]] - 1 # 0.05260194
confint(model32)[[4]] - 1 # 0.1896745
summary(model32)$r.squared # 0.881196
# d) Six Mile __________________________________________________________________
# Before break -----------------------------------------------------------------
model41 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[4]] |> 
                filter(datetime <= BreakPoints[[4]]))
model41$coefficients
10^(model41$coefficients[[1]]) #  0.3747837 g/m3 or mg/l
10^(confint(model41)[[1]]) # 0.2016278
10^(confint(model41)[[3]]) # 0.6966439
model41$coefficients[[2]] - 1 # 0.1148651 b units
confint(model41)[[2]] - 1 # 0.04983291
confint(model41)[[4]] - 1 # 0.1798974
summary(model41)$r.squared # 0.9152105
# After break ------------------------------------------------------------------
model42 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[4]] |> 
                filter(datetime > BreakPoints[[4]]))
model42$coefficients
10^(model42$coefficients[[1]]) # 0.4863233 g/m3 or mg/l
10^(confint(model42)[[1]]) # 0.3039479
10^(confint(model42)[[3]]) # 0.7781279
model42$coefficients[[2]] - 1 # 0.1435439 b units
confint(model42)[[2]] - 1 # 0.09363819
confint(model42)[[4]] - 1 # 0.1934495
summary(model42)$r.squared # 0.9615759
# e) Pennywash _________________________________________________________________
# Before break -----------------------------------------------------------------
model51 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[5]] |> 
                filter(datetime <= BreakPoints[[5]]))
model51$coefficients
10^(model51$coefficients[[1]]) # 0.2329693 g/m3 or mg/l
10^(confint(model51)[[1]]) # 0.1736123
10^(confint(model51)[[3]]) # 0.3126201
model51$coefficients[[2]] - 1 # 0.1156645 b units
confint(model51)[[2]] - 1 # 0.08258545
confint(model51)[[4]] - 1 # 0.1487436
summary(model51)$r.squared # 0.9588385
# After break ------------------------------------------------------------------
model52 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[5]] |> 
                filter(datetime > BreakPoints[[5]]))
model52$coefficients
10^(model52$coefficients[[1]]) # 0.4947367 g/m3 or mg/l
10^(confint(model52)[[1]]) # 0.3264638
10^(confint(model52)[[3]]) # 0.7497443
model52$coefficients[[2]] - 1 # 0.1655439 b units
confint(model52)[[2]] - 1 # 0.1180399
confint(model52)[[4]] - 1 # 0.213048
summary(model52)$r.squared # 0.9492677
# f) Jane Green ________________________________________________________________
# Before break -----------------------------------------------------------------
model61 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[7]] |> 
                filter(datetime <= BreakPoints[[7]]))
model61$coefficients
10^(model61$coefficients[[1]]) # 0.1484593 g/m3 or mg/l
10^(confint(model61)[[1]]) # 0.1071635
10^(confint(model61)[[3]]) # 0.2056684
model61$coefficients[[2]] - 1 # 0.06308069 b units
confint(model61)[[2]] - 1 # 0.02716615
confint(model61)[[4]] - 1 # 0.09899524
summary(model61)$r.squared # 0.9496139
# After break ------------------------------------------------------------------
model62 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[7]] |> 
                filter(datetime > BreakPoints[[7]]))
model62$coefficients
10^(model62$coefficients[[1]]) # 0.2483311 g/m3 or mg/l
10^(confint(model62)[[1]]) # 0.1628749
10^(confint(model62)[[3]]) # 0.378624
model62$coefficients[[2]] - 1 # 0.06941513 b units
confint(model62)[[2]] - 1 # 0.02280548
confint(model62)[[4]] - 1 # 0.1160248
summary(model62)$r.squared # 0.9354303
# g) Crabgrass _________________________________________________________________
# Before break -----------------------------------------------------------------
model71 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[9]] |> 
                filter(datetime <= BreakPoints[[9]]))
model71$coefficients
10^(model71$coefficients[[1]]) # 0.100722 g/m3 or mg/l
10^(confint(model71)[[1]]) # 0.06520305
10^(confint(model71)[[3]]) # 0.1555898
model71$coefficients[[2]] - 1 # -0.004680139 b units
confint(model71)[[2]] - 1 # -0.05341498
confint(model71)[[4]] - 1 # 0.0440547
summary(model71)$r.squared # 0.960668
# After break ------------------------------------------------------------------
model72 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[9]] |> 
                filter(datetime > BreakPoints[[9]]))
model72$coefficients
10^(model72$coefficients[[1]]) # 0.3153879 g/m3 or mg/l
10^(confint(model72)[[1]]) # 0.1907775
10^(confint(model72)[[3]]) #0.5213902
model72$coefficients[[2]] - 1 # 0.1122489 b units
confint(model72)[[2]] - 1 # 0.04881125
confint(model72)[[4]] - 1 # 0.1756866
summary(model72)$r.squared # 0.9080397
# h) Tenmile ___________________________________________________________________
# Before break -----------------------------------------------------------------
model81 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[2]] |> 
                filter(datetime <= BreakPoints[[2]]))
model81$coefficients
10^(model81$coefficients[[1]]) # 0.5145342 g/m3 or mg/l
10^(confint(model81)[[1]]) # 0.3502801
10^(confint(model81)[[3]]) # 0.7558108
model81$coefficients[[2]] - 1 # 0.1773357 b units
confint(model81)[[2]] - 1 # 0.1265439
confint(model81)[[4]] - 1 # 0.2281274
summary(model81)$r.squared # 0.9294833
# After break ------------------------------------------------------------------
model82 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[2]] |> 
                filter(datetime > BreakPoints[[2]]))
model82$coefficients
10^(model82$coefficients[[1]]) # 2.119937 g/m3 or mg/l
10^(confint(model82)[[1]]) # 1.029814
10^(confint(model82)[[3]]) # 4.364021
model82$coefficients[[2]] - 1 # 0.2651206 b units
confint(model82)[[2]] - 1 # 0.1706485
confint(model82)[[4]] - 1 # 0.3595927
summary(model82)$r.squared # 0.8489298
# i) Taylor ____________________________________________________________________
# Before break -----------------------------------------------------------------
model91 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[6]] |> 
                filter(datetime <= BreakPoints[[6]]))
model91$coefficients
10^(model91$coefficients[[1]]) # 0.04700786 g/m3 or mg/l
10^(confint(model91)[[1]]) # 0.02430959
10^(confint(model91)[[3]]) # 0.09089989
model91$coefficients[[2]] - 1 # -0.0458964 b units
confint(model91)[[2]] - 1 # -0.1371755
confint(model91)[[4]] - 1 # 0.04538274
summary(model91)$r.squared # 0.8490827
# After break ------------------------------------------------------------------
model92 <- lm(y ~ x, 
              data = LAmbientCreeksNoOut[[6]] |> 
                filter(datetime > BreakPoints[[6]]))
model92$coefficients
10^(model92$coefficients[[1]]) # 0.2925418 g/m3 or mg/l
10^(confint(model92)[[1]]) # 0.09463254
10^(confint(model92)[[3]]) # 0.9043474
model92$coefficients[[2]] - 1 # 0.1529825 b units
confint(model92)[[2]] - 1 # -0.01055594
confint(model92)[[4]] - 1 # 0.3165209
summary(model92)$r.squared # 0.8775943
###### 3. Get variances and standard deviations ################################
# 3.1 Sigmas for sigma ratio ___________________________________________________
GetSigmas <- function(LAmbientCreeksNoOutXX){
  # compute mu
  MeanLnQ <- LAmbientCreeksNoOutXX |> pull(xln) |> mean() # logtransformed Q
  MeanLnC <- LAmbientCreeksNoOutXX |> pull(`TP-T`) |> log() |> mean() # logtransformed C
  # Compute Moment 1
  m1Q <- LAmbientCreeksNoOutXX |> pull(Qmday) |> mean()
  m1C <- LAmbientCreeksNoOutXX |> pull(`TP-T`) |> mean()
  # Compute Moment 2
  m2Q <- LAmbientCreeksNoOutXX |> pull(Qmday) |> var()
  m2C <- LAmbientCreeksNoOutXX |> pull(`TP-T`) |> var()
  m2Qn <- m2Q + m1Q^2
  m2Cn <- m2C + m1C^2
  # Compute sigma
  SigmalnQ <- (log(m2Qn) - 2*log(m1Q))^0.5
  SigmalnC <- (log(m2Cn) - 2*log(m1C))^0.5
  # summarize
  result <- c(MeanLnQ, MeanLnC, m1Q, m1C, m2Q, m2C, m2Qn, m2Cn, SigmalnQ, SigmalnC)
  return(result)
}
Sigmas1 <- GetSigmas(LAmbientCreeksNoOut[[1]])
Sigmas31 <- GetSigmas(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]))
Sigmas32 <- GetSigmas(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]))
Sigmas81 <- GetSigmas(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]))
Sigmas82 <- GetSigmas(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]))
Sigmas41 <- GetSigmas(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]))
Sigmas42 <- GetSigmas(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]))
Sigmas51 <- GetSigmas(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]))
Sigmas52 <- GetSigmas(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]))
Sigmas71 <- GetSigmas(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]))
Sigmas72 <- GetSigmas(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]))
Sigmas91 <- GetSigmas(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]))
Sigmas92 <- GetSigmas(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]))
Sigmas21 <- GetSigmas(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]))
Sigmas22 <- GetSigmas(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]))
Sigmas61 <- GetSigmas(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]))
Sigmas62 <- GetSigmas(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]))
test <- rbind(Sigmas1, 
              Sigmas31,
              Sigmas81,
              Sigmas41,
              Sigmas51,
              Sigmas71,
              Sigmas91,
              Sigmas21,
              Sigmas61,
              Sigmas32,
              Sigmas82,
              Sigmas42,
              Sigmas52,
              Sigmas72,
              Sigmas92,
              Sigmas22,
              Sigmas62)
# 3.2 CVs for CV ratio _________________________________________________________
GetCVsigma <- function(Data){
  # CVs _____________________________________
  # SD C
  SDC <- Data |> pull(`TP-T`) |> sd()
  # mean C
  MEANC <- Data |> pull(`TP-T`) |> mean()
  # SD Q
  SDQ <- Data |> pull(Qmday) |> sd()
  # mean Q
  MEANQ <- Data |> pull(Qmday) |> mean()
  # sigma(logX) _______________________________
  SigmaLogC <- Data |> pull(`TP-T`) |>  log() |> var()
  SigmaLogQ <- Data |> pull(Qmday) |>  log() |> var()
  # mu(logX) _______________________________
  muLogC <- Data |> pull(`TP-T`) |>  log() |> mean()
  muLogQ <- Data |> pull(Qmday) |>  log() |> mean()
  result <- c(SDC, MEANC, SDQ, MEANQ, SigmaLogC, SigmaLogQ, muLogC, muLogQ)
  return(result)
}
CVSigmas11 <- GetCVsigma(LAmbientCreeksNoOut[[1]])
CVSigmas12 <- GetCVsigma(LAmbientCreeksNoOut[[1]])
CVSigmas31 <- GetCVsigma(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]))
CVSigmas32 <- GetCVsigma(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]))
CVSigmas81 <- GetCVsigma(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]))
CVSigmas82 <- GetCVsigma(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]))
CVSigmas41 <- GetCVsigma(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]))
CVSigmas42 <- GetCVsigma(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]))
CVSigmas51 <- GetCVsigma(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]))
CVSigmas52 <- GetCVsigma(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]))
CVSigmas71 <- GetCVsigma(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]))
CVSigmas72 <- GetCVsigma(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]))
CVSigmas91 <- GetCVsigma(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]))
CVSigmas92 <- GetCVsigma(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]))
CVSigmas21 <- GetCVsigma(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]))
CVSigmas22 <- GetCVsigma(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]))
CVSigmas61 <- GetCVsigma(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]))
CVSigmas62 <- GetCVsigma(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]))
test <- rbind(CVSigmas11, 
              CVSigmas31,
              CVSigmas81,
              CVSigmas41,
              CVSigmas51,
              CVSigmas71,
              CVSigmas91,
              CVSigmas21,
              CVSigmas61,
              CVSigmas12, 
              CVSigmas32,
              CVSigmas82,
              CVSigmas42,
              CVSigmas52,
              CVSigmas72,
              CVSigmas92,
              CVSigmas22,
              CVSigmas62)
# 3.3 Unit test ________________________________________________________________
GetCVsigmaMultUnit <- function(Data){
  Data <- Data |> mutate(
    C1 = `TP-T`,
    C2 = `TP-T`*1000,
    C3 = `TP-T`/1000,
    Q1 = `Q(cfs)`,
    Q2 = Qcms,
    Q3 = Qmday
  )
  # CVs _____________________________________
  # SD C
  SDC1 <- Data |> pull(C1) |> sd()
  SDC2 <- Data |> pull(C2) |> sd()
  SDC3 <- Data |> pull(C3) |> sd()
  # mean C
  MEANC1 <- Data |> pull(C1) |> mean()
  MEANC2 <- Data |> pull(C2) |> mean()
  MEANC3 <- Data |> pull(C3) |> mean()
  # SD Q
  SDQ1 <- Data |> pull(Q1) |> sd()
  SDQ2 <- Data |> pull(Q2) |> sd()
  SDQ3 <- Data |> pull(Q3) |> sd()
  # mean Q
  MEANQ1 <- Data |> pull(Q1) |> mean()
  MEANQ2 <- Data |> pull(Q2) |> mean()
  MEANQ3 <- Data |> pull(Q3) |> mean()
  # CVC
  CVC1 <- SDC1/MEANC1
  CVC2 <- SDC2/MEANC2
  CVC3 <- SDC3/MEANC3
  # CVQ
  CVQ1 <- SDQ1/MEANQ1
  CVQ2 <- SDQ2/MEANQ2
  CVQ3 <- SDQ3/MEANQ3
  # sigma(logX) _______________________________
  SigmaLogC1 <- Data |> pull(C1) |>  log() |> var()
  SigmaLogC2 <- Data |> pull(C2) |>  log() |> var()
  SigmaLogC3 <- Data |> pull(C3) |>  log() |> var()
  # SigmaLogQ <- Data |> pull(Qmday) |>  log() |> var()
  SigmaLogQ1 <- Data |> pull(Q1) |>  log() |> var()
  SigmaLogQ2 <- Data |> pull(Q2) |>  log() |> var()
  SigmaLogQ3 <- Data |> pull(Q3) |>  log() |> var()
  result <- c(CVC1, CVC2, CVC3, CVQ1, CVQ2, CVQ3,
              SigmaLogC1, SigmaLogC2, SigmaLogC3,
              SigmaLogQ1, SigmaLogQ2, SigmaLogQ3)
  return(result)
}
CVSigmas11 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[1]])
CVSigmas12 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[1]])
CVSigmas31 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]))
CVSigmas32 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]))
CVSigmas81 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]))
CVSigmas82 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]))
CVSigmas41 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]))
CVSigmas42 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]))
CVSigmas51 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]))
CVSigmas52 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]))
CVSigmas71 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]))
CVSigmas72 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]))
CVSigmas91 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]))
CVSigmas92 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]))
CVSigmas21 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]))
CVSigmas22 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]))
CVSigmas61 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]))
CVSigmas62 <- GetCVsigmaMultUnit(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]))
test <- rbind(CVSigmas11, 
              CVSigmas31,
              CVSigmas81,
              CVSigmas41,
              CVSigmas51,
              CVSigmas71,
              CVSigmas91,
              CVSigmas21,
              CVSigmas61,
              CVSigmas12, 
              CVSigmas32,
              CVSigmas82,
              CVSigmas42,
              CVSigmas52,
              CVSigmas72,
              CVSigmas92,
              CVSigmas22,
              CVSigmas62)
###### 4. Get variances and standard deviations ################################
# 4.1 Creek 1 __________________________________________________________________
LAmbientCreeksNoOut[[1]] |> pull(Qmday) |> mean()
LAmbientCreeksNoOut[[1]] |> pull(`TP-T`) |> mean()

LAmbientCreeksNoOut[[1]] |> pull(Qmday) |> var()
LAmbientCreeksNoOut[[1]] |> pull(`TP-T`) |> var()

var(LAmbientCreeksNoOut[[1]]$xln)
var(LAmbientCreeksNoOut[[1]]$yln)
var(log(LAmbientCreeksNoOut[[1]]$`TP-T`))
sd(exp(LAmbientCreeksNoOut[[1]]$xln))/mean(exp(LAmbientCreeksNoOut[[1]]$xln))
sd(exp(LAmbientCreeksNoOut[[1]]$yln))/mean(exp(LAmbientCreeksNoOut[[1]]$yln))
sd(LAmbientCreeksNoOut[[1]]$`TP-T`)/mean(LAmbientCreeksNoOut[[1]]$`TP-T`)
cor(LAmbientCreeksNoOut[[1]]$`TP-T`, LAmbientCreeksNoOut[[1]]$Qmday, method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[1]]$xln)),
  mean(exp(LAmbientCreeksNoOut[[1]]$xln)),
  sd(LAmbientCreeksNoOut[[1]]$`TP-T`),
  mean(LAmbientCreeksNoOut[[1]]$`TP-T`))
# 4.2 Creek 2 __________________________________________________________________
var(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(xln))
var(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(xln))
var(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`))
sd(LAmbientCreeksNoOut[[1]]$`TP-T`)/mean(LAmbientCreeksNoOut[[1]]$`TP-T`)
cor(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[3]] |> filter(datetime <= BreakPoints[[3]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[3]] |> filter(datetime > BreakPoints[[3]]) |> pull(`TP-T`)))
# 4.3 Creek 3 __________________________________________________________________
var(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(xln))
var(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(xln))
var(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[8]] |> filter(datetime <= BreakPoints[[8]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[8]] |> filter(datetime > BreakPoints[[8]]) |> pull(`TP-T`)))
# 4.4 Creek 4 __________________________________________________________________
var(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(xln))
var(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(xln))
var(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[4]] |> filter(datetime <= BreakPoints[[4]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[4]] |> filter(datetime > BreakPoints[[4]]) |> pull(`TP-T`)))
# 4.5 Creek 5 __________________________________________________________________
var(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(xln))
var(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(xln))
var(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[5]] |> filter(datetime <= BreakPoints[[5]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[5]] |> filter(datetime > BreakPoints[[5]]) |> pull(`TP-T`)))
# 4.6 Creek 6 __________________________________________________________________
var(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(xln))
var(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(xln))
var(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[7]] |> filter(datetime <= BreakPoints[[7]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[7]] |> filter(datetime > BreakPoints[[7]]) |> pull(`TP-T`)))
# 4.7 Creek 7 __________________________________________________________________
var(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(xln))
var(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(xln))
var(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[9]] |> filter(datetime <= BreakPoints[[9]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[9]] |> filter(datetime > BreakPoints[[9]]) |> pull(`TP-T`)))
# 4.8 Creek 8 __________________________________________________________________
var(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(xln))
var(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(xln))
var(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[2]] |> filter(datetime <= BreakPoints[[2]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[2]] |> filter(datetime > BreakPoints[[2]]) |> pull(`TP-T`)))
# 4.9 Creek 9 __________________________________________________________________
var(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(xln))
var(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`)))
var(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(xln))
var(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(yln))
var(log(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`)))
sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`))
sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(xln)))/mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(xln)))
sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(yln)))/mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(yln)))
sd(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`))/mean(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`))
cor(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(Qmday),
    method = "pearson")
cor(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`),
    LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(Qmday),
    method = "pearson")
c(sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[6]] |> filter(datetime <= BreakPoints[[6]]) |> pull(`TP-T`)))
c(sd(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(xln))),
  mean(exp(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(xln))),
  sd(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`)),
  mean(LAmbientCreeksNoOut[[6]] |> filter(datetime > BreakPoints[[6]]) |> pull(`TP-T`)))
save.image(
  file = 
    "C:/XXX/YYY/ZZZ/2LQResults.RData")
########################### END CODE ###########################################

