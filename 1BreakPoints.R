######################## 1BreakPoints.R ########################################
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
# This code is written to perform the analysis of breakpoints explained in the #
# paper, and to make Fig. 2                                                    #
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
# 0.3.1 Functions to load data -------------------------------------------------
ReadCreekCData <- # Read P concentration (C) ....................
  function(Creek){
    PdataPath <- paste(wd, "1InformationInputs", "1AmbientCreeks",
                       Creek, "C", "1Pfrompdata.csv", sep = "/")
    Pdata <- read_csv(PdataPath,show_col_types = FALSE) 
    Pdata <- Pdata |>
      mutate(SMPL_COLL_DT = as_datetime(
        SMPL_COLL_DT, format = "%m/%d/%Y %H:%M")) |>
      mutate(datetime = date(SMPL_COLL_DT))
  return(Pdata)}
ReadCreekQData <- # Read discharge (Q) ..........................
  function(Creek){
    QdataPath <- paste(wd, "1InformationInputs", "1AmbientCreeks",
                       Creek, "Q", "2QfromRevision.csv", sep = "/")
    Qdata <- read_csv(QdataPath,show_col_types = FALSE) 
    Qdata <- Qdata |>
      mutate(datetime = as_datetime(datetime, format = "%m/%d/%Y"),
             Qcms = `Q(cfs)`/35.3147)
    return(Qdata)} 
ReadCreekBData <- # Read Biosolids data (B) .....................
  function(Creek){
    BdataPath <- paste(wd, "1InformationInputs", "1AmbientCreeks",
                       Creek, "B",
                       "1YearlyAppFromUSJRB_Ambient_WQ_Summary.csv", sep = "/")
    Bdata <- read_csv(BdataPath,show_col_types = FALSE) 
    Bdata <- Bdata |>
      mutate(datetime = ymd(paste0(Year, "-01-01")),
             TPkg = TP*0.453592)
    Bdata <- Bdata |> dplyr::select(datetime, TPkg) #TP is in pounds
    Bdata$datetime <- as.POSIXct(Bdata$datetime)
  return(Bdata)
  }
# 0.3.2 Load data for all creeks -----------------------------------------------
AmbientCreeks <- list("1FortDrumCreek", 
                      "2TenmileCreek", 
                      "3BlueCypressCreek",
                      "4SixMileCreek",
                      "5PennywashCreek",
                      "6TaylorCreek",
                      "7JaneGreenCreek",
                      "8SouthWolfCreek",
                      "9CrabgrassCreek")
CAmbientCreeks <- AmbientCreeks |> map(ReadCreekCData) # C data
QAmbientCreeks <- AmbientCreeks |> map(ReadCreekQData) # Q data
BAmbientCreeks <- AmbientCreeks |> map(ReadCreekBData) # B data
AAmbientCreeks <- list(178000000, # Creek Areas in m^2
                       65460000,
                       257300000,
                       54750000,
                       44550000,
                       205840000,
                       625000000,
                       20860000,
                       78220000)
# 0.3.3 Compute P load when C and Q available ----------------------------------
MatchCQLoad <- # match C and Q and compute load .....................
  function(Area, Cdata, Qdata){
    MatchingPQ <- # Find days when C and Q available
      inner_join(Cdata, Qdata, by = c("datetime" = "datetime"))
    MatchingPQ <- # Compute P load in g/day for 3 analytes
      MatchingPQ |> # and normalized by Area in # g/m2/day
      mutate(`loadTP-T` = (`TP-T`*Qcms)*86400, 
             `loadTP-D` = (`TP-D`*Qcms)*86400, 
             `loadPO4` = (`PO4`*Qcms)*86400) |>
      mutate(`loadTP-TperA` = `loadTP-T`/Area,
             `loadTP-DperA` = `loadTP-D`/Area,
             `loadPO4perA` = `loadPO4`/Area) |>
      filter(Qcms > 0 & is.na(Qcms) == FALSE) # drop NaNs and 0 flows
  return(MatchingPQ)}
LAmbientCreeks <- 
  list(AAmbientCreeks, CAmbientCreeks, QAmbientCreeks) |> pmap(MatchCQLoad)
# PERCENTAGE OF C DATA WITH MATCHING Q VALUE 
sum(sapply(LAmbientCreeks, nrow)) / sum(sapply(CAmbientCreeks, nrow))
###### 1. Preprocess data ######################################################
# 1.1 Discharge (Q) ____________________________________________________________
# 1.1.1 Plot data and log transforms -------------------------------------------
DistributionPlotsQ <- function(MatchingPQ){
  # Original Data
  ggplot(MatchingPQ, aes(x = datetime, y = Qcms)) + geom_point()
  hist(MatchingPQ$Qcms)
  boxplot(MatchingPQ$Qcms)
  print(shapiro.test(MatchingPQ$Qcms)) 
  # Log transform
  ggplot(MatchingPQ, aes(x = datetime, y = log10(Qcms))) + geom_point()
  hist(log10(MatchingPQ$Qcms))
  boxplot(log10(MatchingPQ$Qcms))
  print(shapiro.test(log10(MatchingPQ$Qcms)))
  # Q NaNs
  print(sum(is.na(MatchingPQ$Qcms)))
}
DistributionPlotsQ(LAmbientCreeks[[1]]) # "1FortDrumCreek", 
DistributionPlotsQ(LAmbientCreeks[[2]]) # "2TenmileCreek",
DistributionPlotsQ(LAmbientCreeks[[3]]) # "3BlueCypressCreek",
DistributionPlotsQ(LAmbientCreeks[[4]]) # "4SixMileCreek",
DistributionPlotsQ(LAmbientCreeks[[5]]) # "5PennywashCreek",
DistributionPlotsQ(LAmbientCreeks[[6]]) # "6TaylorCreek",
DistributionPlotsQ(LAmbientCreeks[[7]]) # "7JaneGreenCreek",
DistributionPlotsQ(LAmbientCreeks[[8]]) # "8SouthWolfCreek",
DistributionPlotsQ(LAmbientCreeks[[9]]) # "9CrabgrassCreek"
# 1.1.2 Manage outliers --------------------------------------------------------
DropOutliersQ <- function(MatchingPQ, LimMin, LimMax){
  Q1 <- quantile(log10(MatchingPQ$Qcms), LimMin)
  Q3 <- quantile(log10(MatchingPQ$Qcms), LimMax)
  IQR_value <- Q3 - Q1
  k <- 1.0
  lower_fence <- Q1 - k * IQR_value
  upper_fence <- Q3 + k * IQR_value
  positions_above <- which(log10(MatchingPQ$Qcms) > upper_fence)
  print(positions_above)  
  positions_below <- which(log10(MatchingPQ$Qcms) < lower_fence)
  print(positions_below)
  Rows2Del <- union(positions_above, positions_below)
  if(length(Rows2Del)>0){
    MatchingPQNoOut <- MatchingPQ|> slice(-Rows2Del)
  } else {MatchingPQNoOut <- MatchingPQ}
  ggplot(MatchingPQNoOut, aes(x = datetime, y = Qcms)) + geom_point()
  hist(log10(MatchingPQNoOut$Qcms))
  boxplot(log10(MatchingPQNoOut$Qcms))
  print(shapiro.test(log10(MatchingPQNoOut$Qcms)))
  return(MatchingPQNoOut)
}
LAmbientCreeksNoOut <- list(LAmbientCreeks, 0.025, 0.975) |> pmap(DropOutliersQ)
DistributionPlotsQ(LAmbientCreeksNoOut[[1]]) # "1FortDrumCreek", 
DistributionPlotsQ(LAmbientCreeksNoOut[[2]]) # "2TenmileCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[3]]) # "3BlueCypressCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[4]]) # "4SixMileCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[5]]) # "5PennywashCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[6]]) # "6TaylorCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[7]]) # "7JaneGreenCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[8]]) # "8SouthWolfCreek",
DistributionPlotsQ(LAmbientCreeksNoOut[[9]]) # "9CrabgrassCreek"
# 1.2 Concentration data (C) ___________________________________________________
# 1.2.1 plot data and transforms -----------------------------------------------
DistributionPlotsC <- function(MatchingCQNoOut){
  # Original Data
  ggplot(MatchingCQNoOut, aes(x = datetime, y = `TP-T`)) + geom_point()
  hist(MatchingCQNoOut$`TP-T`)
  boxplot(MatchingCQNoOut$`TP-T`)
  print(shapiro.test(MatchingCQNoOut$`TP-T`))
  # Log transform
  ggplot(MatchingCQNoOut, aes(x = datetime, y = log10(`TP-T`))) + geom_point()
  hist(log10(MatchingCQNoOut$`TP-T`))
  boxplot(log10(MatchingCQNoOut$`TP-T`))
  print(shapiro.test(log10(MatchingCQNoOut$`TP-T`))) 
  # Q NaNs
  print(sum(is.na(MatchingCQNoOut$`TP-T`)))
}
DistributionPlotsC(LAmbientCreeksNoOut[[1]]) # "1FortDrumCreek", 
DistributionPlotsC(LAmbientCreeksNoOut[[2]]) # "2TenmileCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[3]]) # "3BlueCypressCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[4]]) # "4SixMileCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[5]]) # "5PennywashCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[6]]) # "6TaylorCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[7]]) # "7JaneGreenCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[8]]) # "8SouthWolfCreek",
DistributionPlotsC(LAmbientCreeksNoOut[[9]]) # "9CrabgrassCreek"
# 1.2.2 Manage Outliers --------------------------------------------------------
DropOutliersC <- function(MatchingCQNoOut, LimMin, LimMax, Area){
  MatchingCQNoOut <- MatchingCQNoOut |> filter(is.na(`TP-T`) == FALSE)
  Q1 <- quantile(log10(MatchingCQNoOut$`TP-T`), LimMin)
  Q3 <- quantile(log10(MatchingCQNoOut$`TP-T`), LimMax)
  IQR_value <- Q3 - Q1
  k <- 1.0
  lower_fence <- Q1 - k * IQR_value
  upper_fence <- Q3 + k * IQR_value
  positions_above <- which(log10(MatchingCQNoOut$`TP-T`) > upper_fence)
  print(positions_above)  
  positions_below <- which(log10(MatchingCQNoOut$`TP-T`) < lower_fence)
  print(positions_below)
  Rows2Del <- union(positions_above, positions_below)
  if(length(Rows2Del)>0){
    MatchingCQNoOut2 <- MatchingCQNoOut|> slice(-Rows2Del)
  } else {MatchingCQNoOut2 <- MatchingCQNoOut}
  ggplot(MatchingCQNoOut2, aes(x = datetime, y = `TP-T`)) + geom_point()
  hist(log10(MatchingCQNoOut2$`TP-T`))
  boxplot(log10(MatchingCQNoOut2$`TP-T`))
  print(shapiro.test(log10(MatchingCQNoOut2$`TP-T`)))
  MatchingCQNoOut2 <- # Compute Q in m/day
    MatchingCQNoOut2 |> 
    mutate(Qmday = (Qcms/Area)*86400)
  return(MatchingCQNoOut2)
}
LAmbientCreeksNoOut <- 
  list(LAmbientCreeksNoOut, 0.025, 0.975, AAmbientCreeks) |> pmap(DropOutliersC)
###### 2. Do break point analysis ##############################################
# Q: Qmday
# L: loadTP-TperA
# C: TP-T
profetiza <- function(creek, QCL, logt, tit, tres){
  if(QCL == "Q"){
    creek2proph <- creek |> rename(ds = datetime, y = Qmday)
    if(logt == 1){
      creek2proph <- creek2proph |> mutate(y = log10(y))}}
  if(QCL == "C"){
    creek2proph <- creek |> rename(ds = datetime, y = `TP-T`)
    if(logt == 1){
      creek2proph <- creek2proph |> mutate(y = log10(y))}}
  if(QCL == "L"){
    creek2proph <- creek |> rename(ds = datetime, y = `loadTP-TperA`)
    if(logt == 1){
      creek2proph <- creek2proph |> mutate(y = log10(y))}}
  m <- prophet(creek2proph,
               n.changepoints = 25, # default
               changepoint.range=0.999, # consider all time series (no forecast)
               changepoint.prior.scale = 0.05) 
  future <- make_future_dataframe(m, periods = 1)
  fcst <- predict(m, future)
  plt <- plot(m, fcst) + 
    add_changepoints_to_plot(m,
                             threshold = tres) + 
    ylab("TP [mg/l]") + xlab(NULL) + ggtitle(tit) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(), #element_text(size = 8.8),       
          axis.text = element_text(size = 10.0),        # Remove axis text
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
          plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                    face = "bold"))  # Remove the legend
  return(list(plt,fcst,m))} 
Titles <- list("Fort Drum", # 
               "Tenmile", # 
               "Blue Cypress", # 
               "Six Mile", # 
               "Pennywash", #
               "Taylor", # 
               "Jane Green", # 
               "South Wolf", # 
               "Crabgrass") # 
Tresh <- list(0.01,
              0.088,
              0.046,
              0.017,
              0.040,
              0.0905,
              0.065,
              0.076,
              0.020)
BreakPointPlots <- 
  list(LAmbientCreeksNoOut, "C", 0, Titles, Tresh) |> pmap(profetiza)
graph <- 
  (BreakPointPlots[[1]][[1]] + BreakPointPlots[[3]][[1]] + 
     BreakPointPlots[[8]][[1]]) /
  (BreakPointPlots[[4]][[1]] + BreakPointPlots[[5]][[1]] + 
     BreakPointPlots[[7]][[1]]) /
  (BreakPointPlots[[9]][[1]] + BreakPointPlots[[2]][[1]] + 
     BreakPointPlots[[6]][[1]]) 
graph # This is a preliminary version of the plot
# Export the plot as a TIFF image
ggsave("Fig1.tiff", 
       path = 
         "C:/XXX/YYY/ZZZ",
       plot = graph, device = "tiff", 
       width = 9, height = 7, dpi = 600)
###### 3. Get break point values and slopes ####################################
# 3.1 Get significant break points and delta values ____________________________
BreakPoints <- BreakPointPlots |> map(function(bkpnt){
  model <- bkpnt[[3]]
  sigpos <- which.max(abs(model$params$delta))    
  brkdt <- model$changepoints[sigpos]
  delta <- model$params$delta[sigpos]
  return(list(brkdt, delta))
})
# 3.2 Get break points and their significance __________________________________
# 3.2.1 function to get slope and model summary --------------------------------
TimeRegre <- function(DF){
  slope <- # in [mg/l]/year
    coef(lm(`TP-T` ~ as.numeric(datetime), data = DF))[2]*86400*365
  summ <- summary(lm(`TP-T` ~ as.numeric(datetime), data = DF))
  return(list(slope, summ))}
# 3.2.2 Get slopes and model info for all creeks and deltas --------------------
BreakPoints <- map(BreakPoints, ~ .x[[1]])
BreakPoints[[1]] <- numeric(0)
length(BreakPoints[[1]])
RegsAll <- list(LAmbientCreeksNoOut, BreakPoints) |> 
  pmap(function(creek, bkpnts){
    if(length(bkpnts > 0)){
      DF1 <- bkpnts |> map(
        function(bk) {creek |> filter(datetime <= as.Date(bk))}
      )
      DF2 <- bkpnts |> map(
        function(bk) {creek |> filter(datetime >= as.Date(bk))}
      )
      reg1 <- DF1 |> map(
        function(df){TimeRegre(df)}
      )
      reg2 <- DF2 |> map(
        function(df){TimeRegre(df)}
      )
      reg <- list(reg1, reg2)
    } else {
      reg <- TimeRegre(creek)
    }
    return(reg)
  })
save.image(
  file = 
    "C:/XXX/YYY/ZZZ/1BreakpointsResults.RData")
###### 4. Plot Panels of Figure 2 in the paper #################################
GrayDots = rgb(0.7, 0.7, 0.7)
# Panel A ______________________________________________________________________
PanelA <- ggplot(LAmbientCreeksNoOut[[1]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),      
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelA # 205 145
# Panel B ______________________________________________________________________
PanelB <- ggplot(LAmbientCreeksNoOut[[3]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[3]] |> filter(datetime <= as.Date(BreakPoints[[3]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[3]] |> filter(datetime >= as.Date(BreakPoints[[3]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[3]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelB # 205 145
# Panel C ______________________________________________________________________
PanelC <- ggplot(LAmbientCreeksNoOut[[8]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[8]] |> filter(datetime <= as.Date(BreakPoints[[8]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[8]] |> filter(datetime >= as.Date(BreakPoints[[8]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[8]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelC # 205 145
# Panel D ______________________________________________________________________
PanelD <- ggplot(LAmbientCreeksNoOut[[4]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[4]] |> filter(datetime <= as.Date(BreakPoints[[4]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[4]] |> filter(datetime >= as.Date(BreakPoints[[4]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[4]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelD # 205 145
# Panel E ______________________________________________________________________
PanelE <- ggplot(LAmbientCreeksNoOut[[5]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[5]] |> filter(datetime <= as.Date(BreakPoints[[5]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[5]] |> filter(datetime >= as.Date(BreakPoints[[5]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[5]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),      
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelE # 205 145
# Panel F ______________________________________________________________________
PanelF <- ggplot(LAmbientCreeksNoOut[[7]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[7]] |> filter(datetime <= as.Date(BreakPoints[[7]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[7]] |> filter(datetime >= as.Date(BreakPoints[[7]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[7]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelF # 205 145
# Panel G ______________________________________________________________________
PanelG <- ggplot(LAmbientCreeksNoOut[[9]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[9]] |> filter(datetime <= as.Date(BreakPoints[[9]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[9]] |> filter(datetime >= as.Date(BreakPoints[[9]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[9]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),      
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelG # 205 145
# Panel H ______________________________________________________________________
PanelH <- ggplot(LAmbientCreeksNoOut[[2]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[2]] |> filter(datetime <= as.Date(BreakPoints[[2]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[2]] |> filter(datetime >= as.Date(BreakPoints[[2]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[2]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelH # 205 145
# Panel I ______________________________________________________________________
PanelI <- ggplot(LAmbientCreeksNoOut[[6]], aes(x = datetime, y = `TP-T`)) +
  geom_point(color = GrayDots) +  
  geom_smooth(data = LAmbientCreeksNoOut[[6]] |> filter(datetime <= as.Date(BreakPoints[[6]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_smooth(data = LAmbientCreeksNoOut[[6]] |> filter(datetime >= as.Date(BreakPoints[[6]])),
              method = "lm", se = FALSE, color = rgb(0.8, 0, 0)) +
  geom_vline(xintercept = BreakPoints[[6]], linetype = "21", color = rgb(0.8, 0, 0), size = 0.8) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(), #element_text(size = 8.8),       
        axis.text = element_text(size = 10.0),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.ticks.length = unit(1, "pt"),
        axis.line = element_line(linewidth = 0.1),        # Remove axis lines
        axis.text.x = element_text(size = 7.5, margin = margin(t = 0)), # ****
        axis.text.y = element_text(size = 7.5, margin = margin(r = 0)),  # ****
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5), #****
        panel.grid.major = element_blank(),  # ****
        panel.grid.minor = element_blank(),  # ****
        legend.text = element_text(size = 6),
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 11, hjust = 0.1, vjust = 0.80, 
                                  face = "bold")) +  # Remove the legend
  scale_x_datetime(expand = c(0,0))
PanelI # 205 145
##### 5. Time series coverage and CVs ##########################################
GetCovMetrics <- function(DataCatch){
  days <- as.numeric(max(DataCatch$datetime) - min(DataCatch$datetime))
  years <- days / 365
  perc <- (nrow(DataCatch) / 
             as.numeric(max(DataCatch$datetime) - min(DataCatch$datetime)))*100
  result <- c(days, years, perc)
  return(result)
}
CovMetrics <- 
  LAmbientCreeksNoOut |> map(function(DataCatch){GetCovMetrics(DataCatch)})
result_matrix <- do.call(rbind, CovMetrics)
result_df <- as.data.frame(result_matrix)
colnames(result_df) <- c("Days", "Years", "Percentage")
mean(result_df$Percentage)
sd(result_df$Percentage)
mean(result_df$Years)
sd(result_df$Years)
############################# END CODE #########################################