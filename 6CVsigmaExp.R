######################## 6CVsigmaExp.R #########################################
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
# This code is written make the numerical experiment as described in the paper #
# and presented in Fig. 4                                                      #
################################################################################
###### 0. Set working Directory and load libraries and data ####################
# 0.1 Set working directory ____________________________________________________
wd = paste("C:/XXX/YYY/ZZZ", sep = "/")
setwd(wd)
# 1 Do numerical experiment ####################################################
# 1.1 Main Function ____________________________________________________________
getXX <- function(ndots, sdlnC, sdlnQ, intlnC, intlnQ){
  # 1 Establish ranges _________________________________
  sdlnCrange <- c((sdlnC - intlnC), (sdlnC + intlnC))
  sdlnQrange <- c((sdlnQ - intlnQ), (sdlnQ + intlnQ))
  # 2 Generate uniform vectors in ranges _______________
  sdlnCvector <- seq(sdlnCrange[1], sdlnCrange[2],
                     length.out = ndots)
  sdlnQvector <- seq(sdlnQrange[1], sdlnQrange[2],
                     length.out = ndots)
  # 3 Compute CVC and CVQ Eq 8 Jawitz Mitchell ________
  cvCvector <- sqrt( exp(sdlnCvector^2) - 1)
  cvQvector <- sqrt( exp(sdlnQvector^2) - 1)
  # 4 Generate points _________________________________
  VarCVarQratio <- vector()
  cvCcvQratio <- vector()
  for (i in 1:ndots){
    VarCVarQratio <- c(VarCVarQratio,(sdlnCvector/sdlnQvector[i])^2)
    cvCcvQratio <- c(cvCcvQratio, cvCvector/cvQvector[i])
  }
  df1 <- data.frame( VarCVarQratio = VarCVarQratio,
                     cvCcvQratio = cvCcvQratio)
  plot(VarCVarQratio , cvCcvQratio, col="gray", 
       xlim=c(0.000,1.000),ylim=c(0.000,1.000),
       xlab="(sigma_lnC/sigma_lnQ)^2",ylab="CVc/CVq")
  return(df1)
}
# 1.2 Make plot ________________________________________________________________
df1 <- getXX(200, # ndots,
             0.585, #  sdlnC, 
             1.165, # sdlnQ, 
             0.430, # intlnC, 
             0.465) # intlnQ
library(ggplot2)
plt <- ggplot(df1, aes(VarCVarQratio, cvCcvQratio)) +
  geom_point(color = "gray") +
  scale_x_continuous(limits = c(0, 0.60), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.70), expand = c(0,0)) +
  geom_point(data = data.frame( # red dots
    VarCVarQratio = c(
      0.413063074,
      0.144362942,
      0.352117502,
      0.290135902,
      0.15306302,
      0.226126105,
      0.091361466,
      0.266961255,
      0.221609802,
      0.072293916,
      0.461045402,
      0.37337365,
      0.246000694,
      0.237185413,
      0.266309327,
      0.574671442,
      0.266929909
    ),
    cvCcvQratio = c(
      0.50218571,
      0.265617609,
      0.498439425,
      0.397989306,
      0.247235295,
      0.356044204,
      0.176529337,
      0.412615212,
      0.390692807,
      0.188739151,
      0.572683219,
      0.439158407,
      0.324442453,
      0.332165076,
      0.421398708,
      0.656730157,
      0.429315731
    )), 
             aes(VarCVarQratio, cvCcvQratio),
             color = "blue") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.1),
        axis.ticks = element_line(linewidth = 0.2),      
        axis.ticks.length = unit(2, "pt"),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 9), 
        axis.text.y = element_text(size = 9)) 
plt
############################# END CODE #########################################
