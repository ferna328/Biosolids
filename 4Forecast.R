######################## 4Forecast.R ###########################################
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
# This code is written to implement and show the results of the forecast       #
# of the depletion of biosolids legacy phosphorus, as described in the paper   #
# and shown in Fig. 6                                                          #
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
    "C:/XXX/YYY/ZZZ/3LCModelsResults.RData")
###### 1. Model 1: Constant rate ###############################################
# 1.1 Simulation Function ______________________________________________________
CPoolSim <- function(BreakPointsXX, AppLoadsXX,
                     SimYYyearly, ForecastEnd,
                     ModBef, ModAft, group){
  # 1.1.1 Compute base concentration -------------------------------------------
  CBase <- SimYYyearly |> 
    filter(year < year(BreakPointsXX)) |>
    select(C) |> pull() |> mean()
  CBasemin <- SimYYyearly |> 
    filter(year < year(BreakPointsXX)) |>
    select(Cmin) |> pull() |> mean()
  CBasemax <- SimYYyearly |> 
    filter(year < year(BreakPointsXX)) |>
    select(Cmax) |> pull() |> mean()
  # 1.1.2 Get creek breakpoint and P app loads ---------------------------------
  Break <- BreakPointsXX
  Loads <- AppLoadsXX
  # 1.2.3 Compute forecast dataframe -------------------------------------------
  Result <- data.frame(
    # Compute simulation years .................................................
    year = seq(from = min(SimYYyearly$year, na.rm = TRUE), 
               to = ForecastEnd, by = 1)) |>
    mutate(yearadj = as.numeric(year) - 2000) |>
    # Add applications and Q to dataframe ......................................
    mutate(Appkgha = # Applications
             ifelse(row_number() <= length(SimYYyearly$year)-1,
                    ifelse(SimYYyearly$year %in% Loads$year, 
                           Loads$TPkgha[match(SimYYyearly$year, Loads$year)], 
                           0),
                    0),
           Qmyear = # Yearly Q
             ifelse(row_number() <= length(SimYYyearly$year)-1,
                    SimYYyearly$Q, 
                    mean(SimYYyearly$Q, na.rm = TRUE)),
           Qmday = # Daily Q 
             Qmyear/365) |>
    # Add loads exported from previous models ..................................
    mutate(
      Lkghayear = # Mean Load
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               SimYYyearly$L,
               3650 * 10^(ModAft$coefficients[[1]] + 
                            log10(Qmday) * ModAft$coefficients[[2]])),
      Lkghayearmin = # Min Load 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               SimYYyearly$Lmin,
               3650 * 10^( 
                 confint(ModAft)[[1]] +
                   log10(Qmday)*confint(ModAft)[[2]])
               ),
      Lkghayearmax = # Max Load
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               SimYYyearly$Lmax,
               3650 * 10^( 
                 confint(ModAft)[[3]] +
                   log10(Qmday)*confint(ModAft)[[4]])
               )) |>
    # Decompose base and biosolids P exports ...................................
    mutate(
      Lbase = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBase * Qmyear * 10, 
               3650 * 10^( 
                 ModBef$coefficients[[1]] +
                   log10(Qmday)*ModBef$coefficients[[2]])),
      Lbasemin = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBasemin * Qmyear * 10, 
               3650 * 10^( 
                 confint(ModBef)[[1]] +
                   log10(Qmday)*confint(ModBef)[[2]])),
      Lbasemax = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBasemax * Qmyear * 10, 
               3650 * 10^( 
                 confint(ModBef)[[3]] +
                   log10(Qmday)*confint(ModBef)[[4]])),
      Lbio =  Lkghayear - Lbase,
      Lbiomin = case_when(group == 0 ~ Lkghayearmin - Lbasemax,
                          group == 1 ~ 
                            ifelse(Lkghayearmin - Lbasemax > 0,
                                   Lkghayearmin - Lbasemax, 0.001),
                          group == 2 ~ Lkghayearmin - Lbasemin), 
      Lbiomax = case_when(group %in% c(0, 1) ~ Lkghayearmax - Lbasemin,
                          group == 2 ~ Lkghayearmax - Lbasemax) 
    ) |> # kg/hayear
    # Compute dPdt  .....................................................
    mutate(dPdt = Appkgha - Lkghayear,
           dPdtmin = Appkgha - Lkghayearmax,
           dPdtmax = Appkgha - Lkghayearmin,
           dPdtbio = Appkgha - Lbio,
           dPdtbiomin = case_when(group %in% c(0, 1) ~ Appkgha - Lbiomax,
                                  group == 2 ~ ifelse(row_number() <= 
                                                        length(SimYYyearly$year)-1,
                                                      Appkgha - Lbiomax,
                                                      Appkgha - Lbiomin)),
           dPdtbiomax = case_when(group %in% c(0, 1) ~ Appkgha - Lbiomin,
                                  group == 2 ~ ifelse( 
                                    row_number() <= length(SimYYyearly$year)-1,
                                    Appkgha - Lbiomin,
                                    Appkgha - Lbiomax ))
           ) |>
    # Compute Pool Size (P(t)) .................................................
    mutate(P = ifelse(cumsum(dPdt) > 0,
                      cumsum(dPdt),0),
           Pmin = ifelse(cumsum(dPdtmin) > 0,
                         cumsum(dPdtmin), 0),
           Pmax = ifelse(cumsum(dPdtmax) > 0,
                         cumsum(dPdtmax), 0),
           PBio = ifelse(
             row_number() <= length(SimYYyearly$year)-1,
             ifelse(cumsum(dPdtbio) > 0,
                    cumsum(dPdtbio), 0),
             ifelse(cumsum(dPdtbio) > 0,
                    cumsum(dPdtbio), 0)),
           PminBio = ifelse(
             row_number() <= length(SimYYyearly$year)-1,
             ifelse(cumsum(dPdtbiomin) > 0, cumsum(dPdtbiomin), 0),
             case_when(group == 0 ~ 0,
                       group %in% c(1,2) ~ ifelse(cumsum(dPdtbiomin) > 0, 
                                           cumsum(dPdtbiomin), 0))
             ),
           PmaxBio = ifelse(
             row_number() <= length(SimYYyearly$year)-1,
             ifelse(cumsum(dPdtbiomax) > 0, cumsum(dPdtbiomax), 0),
             case_when(group == 0 ~ 3.7831,
                       group %in% c(1,2) ~ ifelse(cumsum(dPdtbiomax) > 0,
                                           cumsum(dPdtbiomax), 0))
             )
           ) |>
    # Compute concentration ....................................................
    mutate(
      Cmgl = case_when(
        group %in% c(0, 1, 2) ~ ifelse(PBio > 0,
                                    (Lkghayear/Qmyear)/10,
                                    ifelse(year < 2018,
                                           (Lkghayear/Qmyear)/10,
                                           SimYYyearly |>
                                             filter(year < year(Break)) |>
                                             select(C) |> pull() |> mean()))
        ),
      Cmglmin = case_when(
        group %in% c(0, 1) ~ ifelse(PminBio > 0,
                                    (Lkghayearmin/Qmyear)/10,
                                    ifelse(year < 2018,
                                           (Lkghayearmin/Qmyear)/10,
                                           SimYYyearly |>
                                             filter(year < year(Break)) |>
                                             select(Cmin) |> pull() |> mean())),
        group == 2 ~ ifelse(PminBio>0, 
                            (Lkghayearmax/Qmyear)/10,
                            ifelse(year < 2018,
                                   (Lkghayearmax/Qmyear)/10,
                                   SimYYyearly |>
                                     filter(year < year(Break)) |>
                                     select(Cmin) |> pull() |> mean()))),
      Cmglmax = case_when(
        group %in% c(0, 1) ~ ifelse(PmaxBio > 0,
                                    (Lkghayearmax/Qmyear)/10,
                                    ifelse(year < 2018,
                                           (Lkghayearmax/Qmyear)/10,
                                           SimYYyearly |>
                                             filter(year < year(Break)) |>
                                             select(Cmax) |> pull() |> mean())),
        group == 2 ~ ifelse(PmaxBio>0,
                            (Lkghayearmin/Qmyear)/10,
                            ifelse(year < 2018,
                                   (Lkghayearmin/Qmyear)/10,
                                   SimYYyearly |>
                                     filter(year < year(Break)) |>
                                     select(Cmax) |> pull() |> mean())))
      )
  return(Result)}
# 1.1.a Fort Drum [[1]] --------------------------------------------------------
Result11 <- CPoolSim(BreakPoints[[2]], AppLoads[[1]],
                     Sim1yearly, ForecastEnd = 3000,
                     model1, model1, 0)
PanAa <- ggplot(Result11 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.1,0.32),
                     breaks = c(0.1, 0.2, 0.3))
PanAa
PanAb <- ggplot(Result11 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,10),
                     breaks = c(0,5,10))
PanAb 
# 1.1.b Blue Cypress [[3]] -----------------------------------------------------
Result21 <- CPoolSim(BreakPoints[[3]], AppLoads[[3]],
                     Sim2yearly, ForecastEnd = 3000,
                     model21, model22, 1)
PanBa <- ggplot(Result21 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.1,0.32),
                     breaks = c(0.1,0.2,0.3))
PanBa
PanBb <- ggplot(Result21 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,55),
                     breaks = c(0, 25, 50))
PanBb 
# 1.1.c South Wolf [[8]] -------------------------------------------------------
Result31 <- CPoolSim(BreakPoints[[8]], AppLoads[[8]],
                     Sim3yearly, ForecastEnd = 3000,
                     model31, model32, 1)
PanCa <- ggplot(Result31 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.1,0.32),
                     breaks = c(0.1,0.2, 0.3))
PanCa
PanCb <- ggplot(Result31 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,55),
                     breaks = c(0, 25, 50))
PanCb 
# 1.1.d Six Mile [[4]]--------------------------------------------------------------
Result41 <- CPoolSim(BreakPoints[[4]], AppLoads[[4]],
                     Sim4yearly, ForecastEnd = 3000,
                     model41, model42, 1)
PanDa <- ggplot(Result41 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.08,0.6),
                     breaks = c(0.1, 0.2, 0.3))
PanDa
PanDb <- ggplot(Result41 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,40),
                     breaks = c(0, 20, 40))
PanDb #
# 1.1.e Pennywash [[5]] --------------------------------------------------------
Result51 <- CPoolSim(BreakPoints[[5]], AppLoads[[5]],
                 Sim5yearly, ForecastEnd = 3000,
                 model51, model52, 1)
PanEa <- ggplot(Result51 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.08,0.3),
                     breaks = c(0.1, 0.2, 0.3))
PanEa
PanEb <- ggplot(Result51 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,140),
                     breaks = c(0, 70, 140))
PanEb 
# 1.1.f Jane Green [[7]] -------------------------------------------------------
Result61 <- CPoolSim(BreakPoints[[7]], AppLoads[[7]],
                 Sim6yearly, ForecastEnd = 3000,
                 model61, model62, 1)
PanFa <- ggplot(Result61 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.08,0.23),
                     breaks = c(0.1, 0.2, 0.3))
PanFa
PanFb <- ggplot(Result61 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,120),
                     breaks = c(0, 60, 120))
PanFb 
# 1.1.g Crabgrass [[9]] --------------------------------------------------------
Result71 <- CPoolSim(BreakPoints[[9]], AppLoads[[9]],
                     Sim7yearly, ForecastEnd = 3000,
                     model71, model72, 1)
PanGa <- ggplot(Result71 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.075,0.30),
                     breaks = c(0.1, 0.2))
PanGa
PanGb <- ggplot(Result71 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,60),
                     breaks = c(0, 30, 60))
PanGb 
# 1.1.h Tenmile [[2]] --------------------------------------------------------
Result81 <- CPoolSim(BreakPoints[[2]], AppLoads[[2]],
                     Sim8yearly, ForecastEnd = 3000,
                     model81, model82, 1)
PanHa <- ggplot(Result81 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.075,0.80),
                     breaks = c(0.2, 0.6))
PanHa
PanHb <- ggplot(Result81 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,160),
                     breaks = c(0, 80, 160))
PanHb 
# 1.1.i Taylor [[6]] --------------------------------------------------------
replacement_values <- Sim9yearlyGap[1, ]
Sim9yearly <- Sim9yearly %>%
  mutate(
    across(everything(), ~ ifelse(is.na(.x),
                                  replacement_values[[cur_column()]], .x)))
Result91 <- CPoolSim(BreakPoints[[6]], AppLoads[[6]],
                     Sim9yearly, ForecastEnd = 3000,
                     model91, model92, 2)
PanIa <- ggplot(Result91 |> filter(year <= 3000), aes(x = as.numeric(yearadj), y = Cmgl)) +
  geom_ribbon(aes(ymin = Cmglmin, ymax = Cmglmax), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = NULL, y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80)) +
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = NULL) +
  scale_y_continuous(limits =c(0.0,0.2),
                     breaks = c(0.1, 0.2))
PanIa #######
PanIb <- ggplot(Result91 |> filter(year <= 3000), aes(x = yearadj, y = PBio)) +
  geom_ribbon(aes(ymin = PminBio, ymax = PmaxBio), fill = "blue", alpha = 0.2) + 
  geom_line(color = "blue") +  # Adds a line to the plot
  labs(x = "Year", y = NULL, title = NULL) +  # Labels for the plot
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title = element_text(size = 8.8),       # Remove axis titles
        axis.text = element_text(size = 8.8),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(margin = margin(t = 0)), # ****
        axis.text.y = element_text(margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80))+
  scale_x_log10(limits = c(1, 1000),
                breaks = c(1,10,100,1000),
                labels = c("2001", "2010", "2100", "3000")) +
  scale_y_continuous(limits =c(0,50),
                     breaks = c(0, 25, 50))
PanIb 
save.image(
  file = 
    "C:/XXX/YYY/ZZZ/4ForecastPart1.RData")
###### 2. Model 2: Exponential Decay ###########################################
load(
  file = 
    "C:/XXX/YYY/ZZZ/4ForecastPart1.RData")
# 2.1 obtain K _________________________________________________________________
# 2.1.0 Main function ----------------------------------------------------------
GetK <- function(ResultX1, BreakP,
                 SimXyearly){
  CBase <- SimXyearly |> 
    filter(year < year(BreakP)) |>
    select(C) |> pull() |> mean()
  CBasemin <- SimXyearly |> 
    filter(year < year(BreakP)) |>
    select(Cmin) |> pull() |> mean()
  CBasemax <- SimXyearly |> 
    filter(year < year(BreakP)) |>
    select(Cmax) |> pull() |> mean()
  
  ResultX1 <- ResultX1 |> mutate(
    Lbase = 
      ifelse(row_number() <= length(SimXyearly$year),
             CBase * Qmyear * 10, NA),
    Lbasemin = 
      ifelse(row_number() <= length(SimXyearly$year),
             CBasemin * Qmyear * 10, NA),
    Lbasemax = 
      ifelse(row_number() <= length(SimXyearly$year),
             CBasemax * Qmyear * 10, NA),
    Lbio =  Lkghayear - Lbase,
    Lbiomin =  Lkghayearmin - Lbasemin,
    Lbiomax =  Lkghayearmax - Lbasemax)
  
  P1 <- ggplot(ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)), 
               aes(x = year, y = Lkghayear)) + #Lkghayear)) + #Lbio)) +  #
    geom_line() +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = Qmyear), color = "red") + ### Q [m/year]
    
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = PBio/100), color = "blue") + ### Pool [kg/ha]/100
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = Lkghayear/PBio), color = "green") + ### L/pool = K [1/year]
                   # Lkghayear/P), color = "green") + ### L/pool = K [1/year]
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = (Lkghayear/Qmyear)/10 ), color = "orange")  + ### L/Q = C [mg/l - g/m3]
                   # (Lkghayear/Qmyear)/10 ), color = "orange")  + ### L/Q = C [mg/l - g/m3]
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = ((Lkghayear/Qmyear))/PBio), color = "magenta") + ### C/Pool = K/Q [1/m]
                    # ((Lkghayear/Qmyear))/P), color = "magenta") + ### C/Pool = K/Q [1/m]
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)),
              aes(x = year, y = ((Lbio/Qmyear))/PBio), color = "gray") + ### C/Pool = K/Q [1/m]
    scale_y_log10()+
    theme_minimal() 
  P2 <- ggplot(ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                  year >= year(BreakP)), 
               aes(x = year, y = Lkghayear)) + #Lkghayear)) + #bio)) +# 
    geom_line() +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = Qmyear), color = "red") +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = PBio/100), color = "blue") +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = Lkghayear/PBio), color = "green") +
                    # Lkghayear/P), color = "green") +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = (Lkghayear/Qmyear)/10 ), color = "orange")  +
                    # (Lkghayear/Qmyear)/10 ), color = "orange")  +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = ((Lkghayear/Qmyear))/PBio), color = "magenta") +
                    # ((Lkghayear/Qmyear))/P), color = "magenta") +
    geom_line(data = ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE),
                                        year >= year(BreakP)),
              aes(x = year, y = ((Lbio/Qmyear))/PBio), color = "gray") +
    scale_y_log10()+
    theme_minimal()
  KoverQ <- ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)) |>
    mutate(KoverQ = ((Lkghayear/Qmyear))/PBio, #[1/m]
           K = KoverQ * Qmyear) |>
    slice_tail(n = 5) |> 
    select(KoverQ) |> pull() |> mean()
  KoverQbio <- ResultX1 |> filter(year <= max(SimXyearly$year, na.rm = TRUE)) |>
    mutate(KoverQ = ((Lbio/Qmyear))/PBio, #[1/m]
           K = KoverQ * Qmyear) |>
    slice_tail(n = 5) |> 
    select(KoverQ) |> pull() |> mean()
    return(list(P1, P2, KoverQ, KoverQbio, ResultX1))
}
# 2.1.1 Get K for all creeks ---------------------------------------------------
K1 <- GetK(Result11, BreakPoints[[2]], Sim1yearly) 
K2 <- GetK(Result21, BreakPoints[[3]], Sim2yearly) 
K3 <- GetK(Result31, BreakPoints[[8]], Sim3yearly) 
K4 <- GetK(Result41, BreakPoints[[4]], Sim4yearly) 
K5 <- GetK(Result51, BreakPoints[[5]], Sim5yearly) 
K6 <- GetK(Result61, BreakPoints[[7]], Sim6yearly) 
K7 <- GetK(Result71, BreakPoints[[9]], Sim7yearly) 
K8 <- GetK(Result81, BreakPoints[[2]], Sim8yearly) 
K9 <- GetK(Result91, BreakPoints[[6]], Sim9yearly) 
# 2.1.2 See K for all creeks ---------------------------------------------------
K1[[1]]
K1[[2]]
K1[[3]]

K2[[1]]
K2[[2]]
K2[[3]]
K2[[4]]

K3[[1]]
K3[[2]]
K3[[3]]
K3[[4]] 

K4[[1]]
K4[[2]]
K4[[3]]
K4[[4]]

K5[[1]]
K5[[2]]
K5[[3]]
K5[[4]] ###

K6[[1]]
K6[[2]]
K6[[3]]
K6[[4]]

K7[[1]]
K7[[2]]
K7[[3]]
K7[[4]]

K8[[1]]
K8[[2]]
K8[[3]]
K8[[4]]

K9[[1]]
K9[[2]]
K9[[3]]
K9[[4]]
# 2.2 Simulate exp decay _______________________________________________________
# 2.2.0 Main function ----------------------------------------------------------
CPoolSim2 <- function(BreakPointsXX, AppLoadsXX,
                     SimYYyearly, ForecastEnd,
                     KX, ModBef, ModAft, group){
  # 2.2.1 Compute base concentration ........................................
  CBase <- SimYYyearly |> 
           filter(year < year(BreakPointsXX)) |>
           select(C) |> pull() |> mean()
  CBasemin <- SimYYyearly |> 
          filter(year < year(BreakPointsXX)) |>
          select(Cmin) |> pull() |> mean()
  CBasemax <- SimYYyearly |> 
          filter(year < year(BreakPointsXX)) |>
          select(Cmax) |> pull() |> mean()
  # 2.2.2 Get creek breakpoint and P loads ..................................
  Break <- BreakPointsXX
  Loads <- AppLoadsXX
  # 2.2.3 Compute forecast dataframe ........................................
  Result <- data.frame(
    # Compute simulation years ------------------------------
    year = seq(from = min(SimYYyearly$year, na.rm = TRUE), 
               to = ForecastEnd, by = 1)) |>
    mutate(yearadj = as.numeric(year) - 2000) |>
    # Add applications and Q to dataframe --------------------
    mutate(Appkgha = # Applications
              ifelse(row_number() <= length(SimYYyearly$year)-1,
                     ifelse(SimYYyearly$year %in% Loads$year, 
                            Loads$TPkgha[match(SimYYyearly$year, Loads$year)], 
                            0),
                     0),
            Qmyear = # Yearly Q
              ifelse(row_number() <= length(SimYYyearly$year)-1,
                     SimYYyearly$Q, 
                     mean(SimYYyearly$Q, na.rm = TRUE)),
            Qmday = # Daily Q 
              Qmyear/365) |>
    # Add load from previous models -------------------------- ****
    mutate(
       Lkghayear = # Mean Load
         ifelse(row_number() <= length(SimYYyearly$year)-1,
                SimYYyearly$L,
                NA),
       Lkghayearmin = # Min Load 
         ifelse(row_number() <= length(SimYYyearly$year)-1,
                SimYYyearly$Lmin,
                NA),
       Lkghayearmax = # Max Load
         ifelse(row_number() <= length(SimYYyearly$year)-1,
                SimYYyearly$Lmax,
                NA)) |>
    # Decompose ----------------------------------------------- ****
    mutate(
      Lbase = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBase * Qmyear * 10, NA),
      Lbasemin = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBasemin * Qmyear * 10, NA),
      Lbasemax = 
        ifelse(row_number() <= length(SimYYyearly$year)-1,
               CBasemax * Qmyear * 10, NA),
      Lbio =  Lkghayear - Lbase,
      Lbiomin = case_when(group == 1 ~ 
                            ifelse(Lkghayearmin - Lbasemax > 0,
                                   Lkghayearmin - Lbasemax, 0.001)),
      Lbiomax =  Lkghayearmax - Lbasemin
      ) |> # kg/hayear
    # Compute dPdt (Eq. 3) ----------------------------------- *****
    mutate(dPdt = Appkgha - Lkghayear,
           dPdtmin = Appkgha - Lkghayearmax,
           dPdtmax = Appkgha - Lkghayearmin,
           dPdtbio = Appkgha - Lbio,
           dPdtbiomin = Appkgha - Lbiomax,
           dPdtbiomax = Appkgha - Lbiomin) |>
    # Compute Pool Size -------------------------------------- ****
    mutate(P = ifelse(cumsum(dPdt) > 0,
                       cumsum(dPdt),0),
           Pmin = ifelse(cumsum(dPdtmin) > 0,
                          cumsum(dPdtmin), 0),
           Pmax = ifelse(cumsum(dPdtmax) > 0,
                          cumsum(dPdtmax), 0),
           PBio = ifelse(cumsum(dPdtbio) > 0,
                          cumsum(dPdtbio),0),
           PminBio = ifelse(cumsum(dPdtbiomin) > 0,
                         cumsum(dPdtbiomin),0),
           PmaxBio = ifelse(cumsum(dPdtbiomax) > 0,
                         cumsum(dPdtbiomax),0)) |>
    mutate(LbiooverPbio = Lbio/PBio, 
           LbiooverPbiomin = Lbiomin/PmaxBio,
           LbiooverPbiomax = Lbiomax/PminBio
           ) |>
    # Compute Concentration  ------------------------------- ****
    mutate(
      Cmgl = ifelse(P>0,
                    (Lkghayear/Qmyear)/10,
                    ifelse(year < 2018,
                           (Lkghayear/Qmyear)/10,
                           SimYYyearly |>
                             filter(year < year(Break)) |>
                             select(C) |> pull() |> mean())),
      Cmglmin = ifelse(P>0,
                       (Lkghayearmin/Qmyear)/10,
                       ifelse(year < 2018,
                              (Lkghayearmin/Qmyear)/10,
                              SimYYyearly |>
                                filter(year < year(Break)) |>
                                select(Cmin) |> pull() |> mean())),
      Cmglmax = ifelse(P>0,
                       (Lkghayearmax/Qmyear)/10,
                       ifelse(year < 2018,
                              (Lkghayearmax/Qmyear)/10,
                              SimYYyearly |>
                                filter(year < year(Break)) |>
                                select(Cmax) |> pull() |> mean())))
  # # Make forecast ....................................................
  KoverQ = KX[[3]]
  KoverQbio = KX[[4]]
  for (i in length(SimYYyearly$year):length(SimYYyearly$year)) {
  # # Estimate Biosolids load 
  Result$Lbio[[i]] = KoverQbio * Result$Qmyear[[i]] * Result$PBio[[i-1]] 
  Result$Lbiomin[[i]] = KoverQbio * Result$Qmyear[[i]] * Result$PminBio[[i-1]]
  Result$Lbiomax[[i]] = KoverQbio * Result$Qmyear[[i]] * Result$PmaxBio[[i-1]]
  # Estimate Biosolids P exports
  Result$dPdtbio[[i]] = Result$Appkgha[[i]] - Result$Lbio[[i]] 
  Result$dPdtbiomin[[i]] = Result$Appkgha[[i]] - Result$Lbiomax[[i]]
  Result$dPdtbiomax[[i]] = Result$Appkgha[[i]] - Result$Lbiomin[[i]]
  # Estimate Bio pool size
  Result$PBio[[i]] = Result$PBio[[i-1]] + Result$dPdtbio[[i]]
  Result$PminBio[[i]] = Result$PminBio[[i-1]] + Result$dPdtbiomin[[i]]
  Result$PmaxBio[[i]] = Result$PmaxBio[[i-1]] + Result$dPdtbiomax[[i]]
  # Estimate C
  Result$Cmgl[[i]] = (Result$Lbio[[i]]/Result$Qmyear[[i]])/10  + CBase
  Result$Cmglmin[[i]] = (Result$Lbiomin[[i]]/Result$Qmyear[[i]])/10 + CBasemin
  Result$Cmglmax[[i]] = (Result$Lbiomax[[i]]/Result$Qmyear[[i]])/10 + CBasemax
  coun <- length(SimYYyearly$year) + 1}
  for (i in coun:length(Result$year)) {
  # # # Estimate load exported
  Result$Lbio[[i]] = KoverQbio * Result$Qmyear[[i]] * Result$PBio[[i-1]]
  Result$Lbiomin[[i]] = ifelse(
    KoverQbio * Result$Qmyear[[i]] * Result$PminBio[[i-1]] > 0,
    KoverQbio * Result$Qmyear[[i]] * Result$PminBio[[i-1]],
    0
  )
  Result$Lbiomax[[i]] = KoverQbio * Result$Qmyear[[i]] * Result$PmaxBio[[i-1]]
  # #    # Estimate change in pool
  Result$dPdtbio[[i]] = Result$Appkgha[[i]] - Result$Lbio[[i]]
  Result$dPdtbiomin[[i]] = Result$Appkgha[[i]] - Result$Lbiomax[[i]]
  Result$dPdtbiomax[[i]] = ifelse(
    Result$Appkgha[[i]] - Result$Lbiomin[[i]] < 0,
    Result$Appkgha[[i]] - Result$Lbiomin[[i]],
    0
  )
  # #    # Estimate pool size
  Result$PBio[[i]] = Result$PBio[[i-1]] + Result$dPdtbio[[i]]
  Result$PminBio[[i]] = ifelse(
    Result$PminBio[[i-1]] + Result$dPdtbiomin[[i]] > 0,
    Result$PminBio[[i-1]] + Result$dPdtbiomin[[i]],
    0
  ) 
    Result$PminBio[[i-1]] + Result$dPdtbiomin[[i]]
  Result$PmaxBio[[i]] = Result$PmaxBio[[i-1]] + Result$dPdtbiomax[[i]]
    # #    # Estimate concentration
  Result$Cmgl[[i]] = (Result$Lbio[[i]]/Result$Qmyear[[i]])/10 + CBase
  Result$Cmglmin[[i]] = (Result$Lbiomin[[i]]/Result$Qmyear[[i]])/10 + CBasemin
  Result$Cmglmax[[i]] = (Result$Lbiomax[[i]]/Result$Qmyear[[i]])/10 + CBasemax
  }
  return(Result)}
# 2.2.a Fort Drum [[1]] --------------------------------------------------------
Result12 <- CPoolSim2(BreakPoints[[2]], AppLoads[[1]],
                     Sim1yearly, ForecastEnd = 10000,
                     K1, model1, model1, 1)
PanAa <- PanAa +
 geom_ribbon(data = Result11 |> filter(year >= 2022),
             aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
 geom_line(data = Result11 |> filter(year >= 2022), 
           aes(y = Cmgl),
           color = "red") 
PanAa
PanAb <- PanAb +
 geom_ribbon(data = Result11 |> filter(year >= 2022),
             aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
 geom_line(data = Result11 |> filter(year >= 2022), 
           aes(y = PBio),
           color = "red")
PanAb
# 2.2.b Blue Cypress [[3]] -----------------------------------------------------
Result22 <- CPoolSim2(BreakPoints[[3]], AppLoads[[3]],
                      Sim2yearly, ForecastEnd = 10000,
                      K2, model21, model22, 1)
PanBa <- PanBa +
  geom_ribbon(data = Result22 |> filter(year >= 2022),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result22 |> filter(year >= 2022), 
            aes(y = Cmgl),
            color = "red") 
PanBa 
PanBb <- PanBb +
  geom_ribbon(data = Result22 |> filter(year >= 2022),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result22 |> filter(year >= 2022), 
            aes(y = PBio),
            color = "red") 
PanBb 
# 2.2.c South Wolf [[8]] -------------------------------------------------------
Result32 <- CPoolSim2(BreakPoints[[8]], AppLoads[[8]],
                      Sim3yearly, ForecastEnd = 10000,
                      K3, model31, model32, 1)
PanCa <- PanCa +
  geom_ribbon(data = Result32 |> filter(year >= 2019),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result32 |> filter(year >= 2019), 
            aes(y = Cmgl),
            color = "red")
PanCa 
PanCb <- PanCb +
  geom_ribbon(data = Result32 |> filter(year >= 2019),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result32 |> filter(year >= 2019), 
            aes(y = PBio),
            color = "red")
PanCb 
# 2.2.d Six Mile [[4]]--------------------------------------------------------------
Result42 <- CPoolSim2(BreakPoints[[4]], AppLoads[[4]],
                      Sim4yearly, ForecastEnd = 10000,
                      K4, model41, model42, 1)
PanDa <- PanDa +
  geom_ribbon(data = Result42 |> filter(year >= 2018),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result42 |> filter(year >= 2018), 
            aes(y = Cmgl),
            color = "red") +
  scale_y_continuous(limits =c(0.1,0.37),
                     breaks = c(0.1, 0.2, 0.3))
PanDa 
PanDb <- PanDb +
  geom_ribbon(data = Result42 |> filter(year >= 2018),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result42 |> filter(year >= 2018), 
            aes(y = PBio),
            color = "red")
PanDb 
# 2.2.e Pennywash [[5]] --------------------------------------------------------
Result52 <- CPoolSim2(BreakPoints[[5]], AppLoads[[5]],
                      Sim5yearly, ForecastEnd = 10000,
                      K5, model51, model52, 1)
PanEa <- PanEa +
  geom_ribbon(data = Result52 |> filter(year >= 2022),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result52 |> filter(year >= 2022), 
            aes(y = Cmgl),
            color = "red")
PanEa
PanEb <- PanEb +
  geom_ribbon(data = Result52 |> filter(year >= 2022),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result52 |> filter(year >= 2022), 
            aes(y = PBio),
            color = "red")
PanEb
# 2.2.f Jane Green [[7]] -------------------------------------------------------
Result62 <- CPoolSim2(BreakPoints[[7]], AppLoads[[7]],
                     Sim6yearly, ForecastEnd = 10000,
                     K6, model61, model62, 1)
PanFa <- PanFa +
  geom_ribbon(data = Result62 |> filter(year >= 2022),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result62 |> filter(year >= 2022), 
            aes(y = Cmgl),
            color = "red")

PanFa
PanFb <- PanFb + 
  geom_ribbon(data = Result62 |> filter(year >= 2022),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) +
  
  geom_line(data = Result62 |> filter(year >= 2022), 
                  aes(y = PBio),
                  color = "red")
PanFb  
# 2.2.g Crabgrass [[9]] --------------------------------------------------------
Result72 <- CPoolSim2(BreakPoints[[9]], AppLoads[[9]],
                      Sim7yearly, ForecastEnd = 10000,
                      K7, model71, model72, 1)
PanGa <- PanGa +
  geom_ribbon(data = Result72 |> filter(year >= 2019),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result72 |> filter(year >= 2019), 
            aes(y = Cmgl),
            color = "red")
PanGa
PanGb <- PanGb +
  geom_ribbon(data = Result72 |> filter(year >= 2019),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result72 |> filter(year >= 2019), 
            aes(y = PBio),
            color = "red")
PanGb
# 2.2.h Tenmile [[2]] --------------------------------------------------------
Result82 <- CPoolSim2(BreakPoints[[2]], AppLoads[[2]],
                      Sim8yearly, ForecastEnd = 10000,
                      K8, model81, model82, 1)
PanHa <- PanHa +
  geom_ribbon(data = Result82 |> filter(year >= 2022),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result82 |> filter(year >= 2022), 
            aes(y = Cmgl),
            color = "red")
PanHa
PanHb <- PanHb +
  geom_ribbon(data = Result82 |> filter(year >= 2022),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result82 |> filter(year >= 2022), 
            aes(y = PBio),
            color = "red")
PanHb
# 2.2.i Taylor [[6]] --------------------------------------------------------
Result92 <- CPoolSim2(BreakPoints[[6]], AppLoads[[6]],
                      Sim9yearly, ForecastEnd = 10000,
                      K9, model91, model92, 1)
PanIa <- PanIa +
  geom_ribbon(data = Result92 |> filter(year >= 2020),
              aes(ymin = Cmglmin, ymax = Cmglmax), fill = "red", alpha = 0.2) + 
  geom_line(data = Result92 |> filter(year >= 2020), 
            aes(y = Cmgl),
            color = "red")
PanIa
PanIb <- PanIb +
  geom_ribbon(data = Result92 |> filter(year >= 2020),
              aes(ymin = PminBio, ymax = PmaxBio), fill = "red", alpha = 0.2) + 
  geom_line(data = Result92 |> filter(year >= 2020), 
            aes(y = PBio),
            color = "red")
PanIb
############################# END CODE #########################################
