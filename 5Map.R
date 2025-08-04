######################## 5Map.R ################################################
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
# This code is written show the Upper St. John's River Basin on a map          #
# as presented in the paper in Fig. 1                                          #
################################################################################
###### 0. Set working Directory and load libraries #############################
wd = "C:/XXX/YYY/ZZZ" # Of. Desktop
setwd(wd)
library(leaflet)
library(sf)
library(nhdplusTools)
library(purrr)
library(dplyr)
library(FedData)
library(soilDB)
library(ggspatial)
library(ggplot2)
###### 1. Read Boundaries and Stream Layers ####################################
# 1.1 River's Water Management District Boundaries _____________________________
WMDFileNames <- c("SJRWMD.shp",
                  "USJRB_Ambient_watersheds.shp",
                  "HSPF_model_watersheds.shp",
                  "Permitted_BS_fields_2018.shp",
                  "Ambient_WQ_Sites.shp",
                  "HSPF_model_reaches.shp")
WMDlayers <- WMDFileNames |>
  map(function(FileNames){
    result <- st_read(file.path( # Data path
      "1InfoBank", "1Boundaries", "0SJRWMD", FileNames))
    result <- st_transform(result, crs = "+proj=longlat +datum=WGS84")
    return(result)
  })
# 1.2 NHD High resolution Data for HUC0308 _____________________________________
hr0308 <- get_nhdplushr( # Get data
  file.path("1InfoBank", "1Boundaries", "1NHDHRHU03", "0308"), 
  layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody", "NHDArea",
             "NHDLine", "NHDPlusBurnWaterbody", "NHDPlusSink", "NHDPlusWall", 
             "NHDPoint", "WBDHU2", "WBDHU4", "WBDHU6", "WBDHU8", "WBDHU10", 
             "WBDHU12"),
  min_size_sqkm = 0,
  simp = 0,
  keep_cols = NULL
)
hr0308 <- hr0308 |> map(function(layer){ # Project all layers
  st_transform(layer, crs = "+proj=longlat +datum=WGS84")})
###### 2. Filter layers for Upper St. Johns (HUC 03080101) #####################
# 2.1 Filter HUC8 ______________________________________________________________
hr03080101 <- hr0308 # Copy previous layers
hr03080101$NHDFlowline <-  # Keep only relevant Flowlines
  hr03080101$NHDFlowline |> 
  filter(substr(REACHCODE, 1, 8) == "03080101")
hr03080101$WBDHU8 <- # keep only relevant HUC 8
  hr03080101$WBDHU8 |>
  filter(hr03080101$WBDHU8$HUC8 == "03080101")
hr03080101$WBDHU10 <- # keep only relevant HUC 10
  hr03080101$WBDHU10 |>
  filter(substr(HUC10, 1, 8) == "03080101")
hr03080101$WBDHU12 <- # keep only relevant HUC 12
  hr03080101$WBDHU12 |>
  filter(substr(HUC12, 1, 8) == "03080101")
hr03080101$NHDPlusBurnWaterbody <- # Keep only relevant lakes
  st_intersection( 
    st_make_valid(hr03080101$NHDPlusBurnWaterbody), 
    st_make_valid(hr03080101$WBDHU8))
# 2.2 Filter HUC10 ______________________________________________________________
HUC10 <- hr03080101$WBDHU10 |> filter(HUC10 %in% c("0308010101",
                                                  "0308010102",
                                                  "0308010103",
                                                  "0308010104",
                                                  "0308010105",
                                                  "0308010106",
                                                  "0308010107"#,
                                                  #"0308010108",
                                                  #"0308010109",
                                                  #"0308010110"
                                                  ))
HUC10 <- st_union(HUC10)
Flowlines <- st_intersection( 
  st_make_valid(hr03080101$NHDFlowline), 
  st_make_valid(HUC10))
Flowlines <- st_intersection( 
  st_make_valid(WMDlayers[[6]]$geometry), 
  st_make_valid(HUC10))
Waterbodies <- st_intersection( 
  st_make_valid(hr03080101$NHDPlusBurnWaterbody), 
  st_make_valid(HUC10))
Fields <- st_intersection( 
  st_make_valid(WMDlayers[[4]]), 
  st_make_valid(HUC10))
Sites <- WMDlayers[[5]] |> filter(!(SHORT_NAME %in% c("XTRIANGLE", "SGO")))
ggplot() +
  geom_sf(data = HUC10, aes(fill = "Region H"), fill = rgb(0.95,0.95,0.95), color = rgb(0.6,0.6,0.6), size = 0.8, lwd = 0.5, show.legend = TRUE) +
  geom_sf(data = Fields, aes(fill = "Region F"), fill = "#E6BBAD", color = "#E6BBAD", size = 0.8, lwd = 0.0, show.legend = TRUE) +
  geom_sf(data = Flowlines, aes(color = "Outline Fl"), fill = NA, color = "#4BAAC8", size = 0.8, lwd = 0.35, show.legend = TRUE) +
  geom_sf(data = Waterbodies, aes(fill = "Region W"),fill = "#4BAAC8", color = "#4BAAC8", size = 0.8, lwd = 0.35, show.legend = TRUE) +
  geom_sf(data = WMDlayers[[2]], aes(color = "test1"), fill = NA, color = "black", size = 0.8, lwd = 1.2, show.legend = TRUE) +
  geom_sf(data = Sites, aes(color = "test2"), fill = NA, color = "red", size = 3, lwd = 0.35, show.legend = TRUE) +
  guides(color = guide_legend(reverse = TRUE)) +
  annotation_scale(location = "br", width_hint = 0.25, style = "ticks"
  ) +
  scale_fill_manual(
    name = "Regions",
    values = c("Region H" = rgb(0.95, 0.95, 0.95), 
               "Region F" = "#E6BBAD", 
               "Region W" = "#4BAAC8")
  ) +
  scale_color_manual(
    name = "Boundaries and Points",
    values = c("Outline Fl" = "#4BAAC8", 
               "WMD Boundaries" = "black", 
               "Site Locations" = "red")) +
  
  theme_minimal() +
  theme(axis.title = element_blank(),       # Remove axis titles
        axis.text = element_blank(),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.line = element_blank(),        # Remove axis lines
        panel.grid = element_blank(),       # Remove grid lines
        #legend.position = c(0.74,0.779),
        legend.text = element_text(size = 7),
        legend.direction = "horizontal",
        legend.key.height = unit(0.25, 'cm'),
        legend.key.width = unit(0.07, 'cm')) 
###### 3. Map in Florida #######################################################
Districts <- st_read(file.path( # Data path
  "1InfoBank", "1Boundaries", 
  "5FloridaDistricts", "States.shp"))
Districts <- st_transform(Districts, crs = "+proj=longlat +datum=WGS84")
Florida <- Districts |> filter(STATE_NAME == "Florida")
plot(Florida$geometry)

ggplot() +
  geom_sf(data = HUC10, fill = rgb(0.9,0.9,0.9), color = rgb(0.5,0.5,0.5), size = 0.8, lwd = 0.5) +
  geom_sf(data = Florida, fill = NA, color = "black", size = 0.8, lwd = 0.35) +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.25, style = "ticks"
  ) +
  theme(axis.title = element_blank(),       # Remove axis titles
        axis.text = element_blank(),        # Remove axis text
        axis.ticks = element_blank(),       # Remove axis ticks
        axis.line = element_blank(),        # Remove axis lines
        panel.grid = element_blank(),       # Remove grid lines
        legend.position = c(0.74,0.779),
        legend.text = element_text(size = 7),
        legend.direction = "horizontal",
        legend.key.height = unit(0.25, 'cm'),
        legend.key.width = unit(0.07, 'cm'))
########### END CODE ###########################################################
