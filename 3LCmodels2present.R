######################## 3LCmodels2present.R ###################################
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
# This code is written to implement and show the results of the load and       #
# concentration models to the present time, namely when biosolids applications #
# stopped as described in the paper and shown in Fig. 5                        #
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
    "C:/XXX/YYY/ZZZ/2LQResults.RData")
###### 1. Figure 5: Biosolids applications subpanels ###########################
# 1.1 Prepare Application Loads ________________________________________________
AppLoads <- list(BAmbientCreeks, AAmbientCreeks) |> 
  pmap(function(apps, areas){
  apps |> mutate(year = year(datetime),
                 TPkgha = (TPkg/areas)*10000)
})
# 1.2 Make plots _______________________________________________________________
AppPlots <- AppLoads |> 
  map(function(data){
    plot <- ggplot(data, aes(x = year, y = TPkgha)) + 
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = NULL, y = NULL, title = NULL) +
      theme(legend.position = "none",
            axis.title = element_text(size = 8.8),       # Remove axis titles
            axis.text = element_text(size = 8.8),        # Remove axis text
            axis.ticks = element_blank(),       # Remove axis ticks
            axis.ticks.length = unit(1, "pt"),
            axis.line = element_line(linewidth = 0.1),        # Remove axis lines
            axis.text.x = element_text(margin = margin(t = 0)), # ****
            axis.text.y = element_text(margin = margin(r = 0)),  # ****
            plot.margin = margin(t = 5, r = 0, b = 0, l = 0), #****
            panel.grid.major = element_blank(),  # ****
            panel.grid.minor = element_blank(),  # ****
            legend.text = element_text(size = 6),
            legend.direction = "horizontal",
            legend.key.height = unit(0.4, 'cm'),
            legend.key.width = unit(0.4, 'cm'),
            legend.title = element_blank(), #element_text(size = 9),
            plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
      scale_x_continuous(limits =c(1999,2023),
                         breaks = c(2000, 2010, 2020))})
# 1.3 Adjust plots _______________________________________________________________
AppPlots[[1]] <- AppPlots[[1]] +
  scale_y_continuous(limits =c(0,1),
                     breaks = c(0, 0.5, 1))
AppPlots[[1]]
AppPlots[[3]] <- AppPlots[[3]] +
  scale_y_continuous(limits =c(0,10),
                     breaks = c(0, 5, 10))
AppPlots[[3]]
AppPlots[[8]] <- AppPlots[[8]] +
  scale_y_continuous(limits =c(0,20),
                     breaks = c(0, 10, 20))
AppPlots[[8]]
AppPlots[[4]] <- AppPlots[[4]] +
  scale_y_continuous(limits =c(0,10),
                     breaks = c(0, 5, 10))
AppPlots[[4]]
AppPlots[[5]] <- AppPlots[[5]] +
  scale_y_continuous(limits =c(0,20),
                     breaks = c(0, 10, 20))
AppPlots[[5]]
AppPlots[[7]] <- AppPlots[[7]] +
  scale_y_continuous(limits =c(0,20),
                     breaks = c(0, 10, 20))
AppPlots[[7]]
AppPlots[[9]] <- AppPlots[[9]] +
  scale_y_continuous(limits =c(0,20),
                     breaks = c(0, 10, 20))
AppPlots[[9]]
AppPlots[[2]] <- AppPlots[[2]] +
  scale_y_continuous(limits =c(0,30),
                     breaks = c(0, 15, 30))
AppPlots[[2]]
AppPlots[[6]] <- AppPlots[[6]] +
  scale_y_continuous(limits =c(0,20),
                     breaks = c(0, 10, 20))
AppPlots[[6]]
###### 2. Compute observed C yearly Data #######################################
ObsC <- LAmbientCreeksNoOut |>
  map(function(creek){
    Result <- creek |> mutate(year = year(SMPL_COLL_DT)) |>
      group_by(year) |>                        
      summarize(avg = mean(`TP-T`, na.rm = TRUE),
                n = n())
    return(Result)
  })
Taylor1 <- ObsC[[6]]
Taylor2 <- LAmbientCreeksNoOut[[6]]
###### 3. Assess Q time series #################################################
# 3.1 Q data summary ___________________________________________________________
ObsQSummary <- QAmbientCreeks |>
  map(function(creek){
    Result <- creek |>
      mutate(year = year(datetime)) |>
      group_by(year) |>
      summarise(
        zero_count = sum(Qcms == 0, na.rm = TRUE), 
        na_count = sum(is.na(Qcms)),
        n = n()) |>
      filter(year >= 2000,
             year < 2023) |>
      mutate(Perc0 = (zero_count/n)*100)
  })
test1 <- ObsQSummary[[1]] # No NaN # a) Fort Drum
test2 <- ObsQSummary[[2]] # No NaN # b
test3 <- ObsQSummary[[3]] # No NaN # b) Blue Cypress
test4 <- ObsQSummary[[4]] # 132 NaN 2019 d) six mile
test5 <- ObsQSummary[[5]] # > 50 NaN 2016, 2017 and 2018
test6 <- ObsQSummary[[6]] # No NaN
test7 <- ObsQSummary[[7]] # No NaN
test8 <- ObsQSummary[[8]] # no NaN # c) South Wolf
test9 <- ObsQSummary[[9]] # no zeros no NaN: perfect
# a) Fort Drum _________________________________________________________________
ggplot(QAmbientCreeks[[1]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[1]] |> filter(Qcms == 0),
            aes(x = datetime, 
                y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# b) Fort Drum _________________________________________________________________
ggplot(QAmbientCreeks[[3]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[3]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# c) South Wolf ________________________________________________________________
ggplot(QAmbientCreeks[[8]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[8]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# d) Six Mile __________________________________________________________________
ggplot(QAmbientCreeks[[4]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[4]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# e) Pennywash _________________________________________________________________
ggplot(QAmbientCreeks[[5]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[5]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# f) Jane Green ________________________________________________________________
ggplot(QAmbientCreeks[[7]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[7]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# g) Crabgrass _________________________________________________________________
ggplot(QAmbientCreeks[[9]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[9]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Perfect
# h) Tenmile ___________________________________________________________________
ggplot(QAmbientCreeks[[2]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[2]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
# Conclusion: zeros can be zeros or near zeros
# i) Taylor ____________________________________________________________________
ggplot(QAmbientCreeks[[6]], aes(x = datetime, y = Qcms)) +
  geom_line() +
  geom_point(data = QAmbientCreeks[[6]] |> filter(Qcms == 0),
             aes(x = datetime, 
                 y = Qcms), color = "red", size = 1.5)
###### 4. Make C and L subpanels Fig 5 #########################################
Qdata <- list(QAmbientCreeks, AAmbientCreeks) |> 
  pmap(function(Q, areas){
    Result <- Q |> mutate(
      Qmday = (Qcms/areas)*86400)})
test <- Qdata[[6]]
# a) Fort Drum _________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim1daily <- Qdata[[1]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[1]]$Qmday[Qdata[[1]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[1]]$Qmday[Qdata[[1]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = 10^(
      model1$coefficients[[1]] + log10(Qmday)*model1$coefficients[[2]]),
    Lgm2dayMin = 10^(
      confint(model1)[[1]] + log10(Qmday)*confint(model1)[[2]]),
    Lgm2dayMax = 10^(
      confint(model1)[[3]] + log10(Qmday)*confint(model1)[[4]]),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim1yearly <- Sim1daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10, 
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs1yearly <- ObsC[[1]] |>
  left_join(Sim1yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
La <- ggplot(Sim1yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs1yearly, 
             aes(x = as.numeric(year), 
                y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(),# element_text(margin = margin(r = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,1.0),
                     breaks = c(0, 0.5, 1.0))
La
Ca <- ggplot(Sim1yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs1yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(),#element_text(margin = margin(r = 0)), #element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.4),
                     breaks = c(0, 0.2, 0.4))
Ca
# b) Blue Cypress ______________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim2daily <- Qdata[[3]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[3]]$Qmday[Qdata[[3]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[3]]$Qmday[Qdata[[3]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[3]] ~
        10^(
          model21$coefficients[[1]] + log10(Qmday)*model21$coefficients[[2]]),
      datetime > BreakPoints[[3]] ~  
        10^(
          model22$coefficients[[1]] + log10(Qmday)*model22$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[3]] ~ 
        10^(
          confint(model21)[[1]] + log10(Qmday)*confint(model21)[[2]]),
      datetime > BreakPoints[[3]] ~ 
        10^(
          confint(model22)[[1]] + log10(Qmday)*confint(model22)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[3]] ~ 
        10^(
          confint(model21)[[3]] + log10(Qmday)*confint(model21)[[4]]),
      datetime > BreakPoints[[3]] ~ 
        10^(
          confint(model22)[[3]] + log10(Qmday)*confint(model22)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim2yearly <- Sim2daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10,  
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs2yearly <- ObsC[[3]] |>
  left_join(Sim2yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
Lb <- ggplot(Sim2yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs2yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,1.6),
                     breaks = c(0, 0.8, 1.6))
Lb
Cb <- ggplot(Sim2yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs2yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.4),
                     breaks = c(0, 0.2, 0.4))
Cb
# c) South Wolf ________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim3daily <- Qdata[[8]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[8]]$Qmday[Qdata[[8]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[8]]$Qmday[Qdata[[8]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[8]] ~
        10^(
          model31$coefficients[[1]] + log10(Qmday)*model31$coefficients[[2]]),
      datetime > BreakPoints[[8]] ~  
        10^(
          model32$coefficients[[1]] + log10(Qmday)*model32$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[8]] ~ 
        10^(
          confint(model31)[[1]] + log10(Qmday)*confint(model31)[[2]]),
      datetime > BreakPoints[[8]] ~ 
        10^(
          confint(model32)[[1]] + log10(Qmday)*confint(model32)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[8]] ~ 
        10^(
          confint(model31)[[3]] + log10(Qmday)*confint(model31)[[4]]),
      datetime > BreakPoints[[8]] ~ 
        10^(
          confint(model32)[[3]] + log10(Qmday)*confint(model32)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim3yearly <- Sim3daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10, 
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs3yearly <- ObsC[[8]] |>
  left_join(Sim3yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
Lc <- ggplot(Sim3yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs3yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(),  #element_text(margin = margin(r = 0)),  #element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,2.2),
                     breaks = c(0, 1.1, 2.2))
Lc
Cc <- ggplot(Sim3yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs3yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.4),
                     breaks = c(0, 0.2, 0.4))
Cc
# d) Six Mile __________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim4daily <- Qdata[[4]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[4]]$Qmday[Qdata[[4]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[4]]$Qmday[Qdata[[4]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[4]] ~
        10^(
          model41$coefficients[[1]] + log10(Qmday)*model41$coefficients[[2]]),
      datetime > BreakPoints[[4]] ~  
        10^(
          model42$coefficients[[1]] + log10(Qmday)*model42$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[4]] ~ 
        10^(
          confint(model41)[[1]] + log10(Qmday)*confint(model41)[[2]]),
      datetime > BreakPoints[[4]] ~ 
        10^(
          confint(model42)[[1]] + log10(Qmday)*confint(model42)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[4]] ~ 
        10^(
          confint(model41)[[3]] + log10(Qmday)*confint(model41)[[4]]),
      datetime > BreakPoints[[4]] ~ 
        10^(
          confint(model42)[[3]] + log10(Qmday)*confint(model42)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim4yearly <- Sim4daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10, 
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs4yearly <- ObsC[[4]] |>
  left_join(Sim4yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
Ld <- ggplot(Sim4yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs4yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,1.4),
                     breaks = c(0, 0.7, 1.4))
Ld
Cd <- ggplot(Sim4yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs4yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.4),
                     breaks = c(0, 0.2, 0.4))
Cd
# e) Pennywash _________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim5daily <- Qdata[[5]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[5]]$Qmday[Qdata[[5]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[5]]$Qmday[Qdata[[5]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[5]] ~
        10^(
          model51$coefficients[[1]] + log10(Qmday)*model51$coefficients[[2]]),
      datetime > BreakPoints[[5]] ~  
        10^(
          model52$coefficients[[1]] + log10(Qmday)*model52$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[5]] ~ 
        10^(
          confint(model51)[[1]] + log10(Qmday)*confint(model51)[[2]]),
      datetime > BreakPoints[[5]] ~ 
        10^(
          confint(model52)[[1]] + log10(Qmday)*confint(model52)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[5]] ~ 
        10^(
          confint(model51)[[3]] + log10(Qmday)*confint(model51)[[4]]),
      datetime > BreakPoints[[5]] ~ 
        10^(
          confint(model52)[[3]] + log10(Qmday)*confint(model52)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim5yearly <- Sim5daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10, 
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs5yearly <- ObsC[[5]] |>
  left_join(Sim5yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------
Le <- ggplot(Sim5yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs5yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), #element_text(margin = margin(r = 0)),  #element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,2.2),
                     breaks = c(0, 1.1, 2.2))
Le
Ce <- ggplot(Sim5yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs5yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.4),
                     breaks = c(0, 0.2, 0.4))
Ce
# f) Jane Green ________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim6daily <- Qdata[[7]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[7]]$Qmday[Qdata[[7]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[7]]$Qmday[Qdata[[7]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[7]] ~
        10^(
          model61$coefficients[[1]] + log10(Qmday)*model61$coefficients[[2]]),
      datetime > BreakPoints[[7]] ~  
        10^(
          model62$coefficients[[1]] + log10(Qmday)*model62$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[7]] ~ 
        10^(
          confint(model61)[[1]] + log10(Qmday)*confint(model61)[[2]]),
      datetime > BreakPoints[[7]] ~ 
        10^(
          confint(model62)[[1]] + log10(Qmday)*confint(model62)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[7]] ~ 
        10^(
          confint(model61)[[3]] + log10(Qmday)*confint(model61)[[4]]),
      datetime > BreakPoints[[7]] ~ 
        10^(
          confint(model62)[[3]] + log10(Qmday)*confint(model62)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim6yearly <- Sim6daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10,   
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs6yearly <- ObsC[[7]] |>
  left_join(Sim6yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
Lf <- ggplot(Sim6yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs6yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,1.2),
                     breaks = c(0, 0.6, 1.2))
Lf
Cf <- ggplot(Sim6yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs6yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.25),
                     breaks = c(0, 0.1, 0.2))
Cf
# g) Crabgrass _________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim7daily <- Qdata[[9]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[9]]$Qmday[Qdata[[9]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[9]]$Qmday[Qdata[[9]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[9]] ~
        10^(
          model71$coefficients[[1]] + log10(Qmday)*model71$coefficients[[2]]),
      datetime > BreakPoints[[9]] ~  
        10^(
          model72$coefficients[[1]] + log10(Qmday)*model72$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[9]] ~ 
        10^(
          confint(model71)[[1]] + log10(Qmday)*confint(model71)[[2]]),
      datetime > BreakPoints[[9]] ~ 
        10^(
          confint(model72)[[1]] + log10(Qmday)*confint(model72)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[9]] ~ 
        10^(
          confint(model71)[[3]] + log10(Qmday)*confint(model71)[[4]]),
      datetime > BreakPoints[[9]] ~ 
        10^(
          confint(model72)[[3]] + log10(Qmday)*confint(model72)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim7yearly <- Sim7daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10,   
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs7yearly <- ObsC[[9]] |>
  left_join(Sim6yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------
Lg <- ggplot(Sim7yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs7yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,1.2),
                     breaks = c(0, 0.6, 1.2))
Lg
Cg <- ggplot(Sim7yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs7yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.25),
                     breaks = c(0, 0.1, 0.2))
Cg
# h) Tenmile ___________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim8daily <- Qdata[[2]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ min(
      Qdata[[2]]$Qmday[Qdata[[2]]$Qmday != 0], na.rm = TRUE),
    Qmday == 0 ~ min(
      Qdata[[2]]$Qmday[Qdata[[2]]$Qmday != 0], na.rm = TRUE),
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[2]] ~
        10^(
          model81$coefficients[[1]] + log10(Qmday)*model81$coefficients[[2]]),
      datetime > BreakPoints[[2]] ~  
        10^(
          model82$coefficients[[1]] + log10(Qmday)*model82$coefficients[[2]])),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[2]] ~ 
        10^(
          confint(model81)[[1]] + log10(Qmday)*confint(model81)[[2]]),
      datetime > BreakPoints[[2]] ~ 
        10^(
          confint(model82)[[1]] + log10(Qmday)*confint(model82)[[2]])),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[2]] ~ 
        10^(
          confint(model81)[[3]] + log10(Qmday)*confint(model81)[[4]]),
      datetime > BreakPoints[[2]] ~ 
        10^(
          confint(model82)[[3]] + log10(Qmday)*confint(model82)[[4]])),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim8yearly <- Sim8daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10,   
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs8yearly <- ObsC[[2]] |>
  left_join(Sim8yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
# Plot -------------------------------------------------------------------------  
Lh <- ggplot(Sim8yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs8yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,5.8),
                     breaks = c(0, 2.9, 5.8))
Lh
Ch <- ggplot(Sim8yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs8yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.9),
                     breaks = c(0, 0.4, 0.8))
Ch
# i) Taylor ____________________________________________________________________
# Prepare ----------------------------------------------------------------------
Sim9daily <- Qdata[[6]] |>
  mutate(Qmday = case_when(
    is.na(Qmday) == TRUE ~ case_when(
      as.numeric(year(datetime)) %in% c(2016, 2017, 2018) ~ NA,
      TRUE ~ 0),
    Qmday == 0 ~ 0,
    TRUE ~ Qmday
  )) |>
  mutate(
    Lgm2day = case_when(
      datetime <= BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          model91$coefficients[[1]] + log10(Qmday)*model91$coefficients[[2]])),
      datetime > BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          model92$coefficients[[1]] + log10(Qmday)*model92$coefficients[[2]]))),
    Lgm2dayMin = case_when(
      datetime <= BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          confint(model91)[[1]] + log10(Qmday)*confint(model91)[[2]])),
      datetime > BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          confint(model92)[[1]] + log10(Qmday)*confint(model92)[[2]]))),
    Lgm2dayMax = case_when(
      datetime <= BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          confint(model91)[[3]] + log10(Qmday)*confint(model91)[[4]])),
      datetime > BreakPoints[[6]] ~ if_else(
        Qmday == 0, 0,
        10^(
          confint(model92)[[3]] + log10(Qmday)*confint(model92)[[4]]))),
    Cmgl = Lgm2day/Qmday,
    Cmglmin = Lgm2dayMin/Qmday,
    Cmglmax = Lgm2dayMax/Qmday) 
Sim9yearly <- Sim9daily |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(Q = sum(Qmday),
            L = sum(Lgm2day)*10,
            Lmin = sum(Lgm2dayMin)*10,
            Lmax = sum(Lgm2dayMax)*10,   
            C = (L/Q)/10,
            Cmin = (Lmin/Q)/10,
            Cmax = (Lmax/Q)/10)
Obs9yearly <- ObsC[[6]] |>
  left_join(Sim9yearly |> select(year, Q), by = "year") |>
  rename(C = avg) |>
  mutate(L = C*Q*10)
Sim9yearlyGap <- Sim9yearly |> filter(is.na(Q) == TRUE) |>
  mutate(Q = mean(Sim9yearly$Q, na.rm = TRUE),
         L = 3650 * 10^(model92$coefficients[[1]] + 
                          log10((Q/365)) * model92$coefficients[[2]]),
         Lmax = 3650 * 10^(confint(model92)[[1]] + 
                             log10((Q/365))*confint(model92)[[2]]),
         Lmin = 3650 * 10^(confint(model92)[[3]] + 
                             log10((Q/365))*confint(model92)[[4]]),
         C = (L/Q)/10,
         Cmin = (Lmin/Q)/10,
         Cmax = (Lmax/Q)/10)

Sim9yearlyGap <- bind_rows(Sim9yearlyGap, Sim9yearly |> filter(year %in% c(2015, 2019)))
# Plot -------------------------------------------------------------------------  
Li <- ggplot(Sim9yearly |> filter(year>=2000), aes(x = year, y = L, group = 1)) +
  geom_ribbon(aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs9yearly, 
             aes(x = as.numeric(year), 
                 y = L), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.6),
                  breaks = c(0, 0.3, 0.6)) +
  geom_line(data = Sim9yearlyGap,
            aes(x = as.numeric(year), 
                y = L),
            color = "blue",
            linetype = "dashed") +
  geom_ribbon(data = Sim9yearlyGap,
              aes(ymin = Lmin, ymax = Lmax), fill = "blue", alpha = 0.2)
Li
Ci <- ggplot(Sim9yearly |> filter(year>=2000), aes(x = year, y = C, group = 1)) +
  geom_ribbon(aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2) +  
  geom_line(color = "blue") +
  geom_point(data = Obs9yearly, 
             aes(x = as.numeric(year), 
                 y = C), color = "red", size = 1.5) +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_blank(), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.5, vjust = 0.80, 
                                  face = "bold")) +
  scale_x_continuous(limits =c(2000,2023),
                     breaks = c(2000, 2010, 2020)) +
  scale_y_continuous(limits =c(0,0.3),
                   breaks = c(0, 0.1, 0.2)) +
  geom_line(data = Sim9yearlyGap,
            aes(x = as.numeric(year), 
                y = C),
            color = "blue",
            linetype = "dashed") +
  geom_ribbon(data = Sim9yearlyGap,
              aes(ymin = Cmin, ymax = Cmax), fill = "blue", alpha = 0.2)
Ci
save.image(
  file = 
    "C:/XXX/YYY/ZZZ/3LCModelsResults.RData")
############################# END CODE #########################################
