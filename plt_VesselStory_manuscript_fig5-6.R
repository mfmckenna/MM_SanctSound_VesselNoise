
rm(list=ls())

# PLOTS 
library(ggplot2)
library(plyr)
library(dplyr)
library(plotrix)
library(viridis)

range01 = function(x){(x-min(x))/(max(x)-min(x))}


inDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_VesselManuscript"

# LOAD RESULTS by Site and combine by sites ####
inFilesTOL = list.files(inDir,pattern = "outputTOL",full.names = T)
load(inFilesTOL[2])
TOL_SB03 = outputTOL[,5:ncol(outputTOL)]
load(inFilesTOL[1])
TOL_GR01 = outputTOL[,2:ncol(outputTOL)]
outputTOL = as.data.frame ( rbind(TOL_SB03, TOL_GR01) )

inFilesVD = list.files(inDir,pattern = "outputVD",full.names = T)
load(inFilesVD[2])
VD_SB03 =outputVD
load(inFilesVD[1])
VD_GR01 = outputVD
outputVD = as.data.frame( rbind(VD_SB03, VD_GR01) )

# SUMMARY PLOTS ####
ggplot(outputVD, aes( y=`TOL_125 max`, color=(Category))  )+
  geom_boxplot()+
  ggtitle (paste0("Variation in sound level- ", outputVD$Sanctuary ) )+
  theme_minimal()+
  facet_wrap(~Sanctuary)
# aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Sanctuary, outputVD$Category), mean, na.rm=T)

ggplot(outputTOL, aes( y=TOL_125, color=(Label))  )+
  geom_boxplot()+
  ggtitle (paste0("Variation in sound level- ", outputTOL$Sant ) )+
  theme_minimal()+
  facet_wrap(~Sant)
# aggregate(outputTOL$TOL_125, by=list(outputTOL$Sant, outputTOL$Label), mean, na.rm=T)

# Seasonal Patterns ####
#percent time with vessel or not by site + month
TOLc = as.data.frame( outputTOL %>% group_by(Sant, Label, mth) %>% tally() ) # TOTAL minutes per month + condition + site
# ND = as.data.frame( TOLc %>% group_by(mth, Sant,Label) %>% mutate(perTime = n/sum(n) ) )
TOLcT = as.data.frame( outputTOL %>% group_by(Sant, mth) %>% tally() )      # TOTAL minutes per month + site
ND = merge(TOLc, TOLcT, by = c("Sant", "mth"))
ND$perTime = ND$n.x/ ND$n.y
# check: 22424/ (22424 + 13334 )

ND = ND[ ND$Label == "A. Vessel Detection", ]
ND$slow = "No"
ND$slow [ ND$mth == 3 | ND$mth == 4] = "Slowdown"

# SIZE METRIC = SNR
# difference for each VD period from closest ambient period (median and max)
## using Vessel DETECTIONS ####
outputVD3 = outputVD[ outputVD$Label == "ship" ,]
outputAB  = outputVD[ outputVD$Label == "ambient",]
outputVD3$SNR = NA
outputVD3$SNRmax = NA
outputVD3$SNRtime = NA

for (ii in 1 : nrow(outputVD3)) {
  
  #only AB for specific sanctuary site
  tmpAB = outputAB[ outputAB$Sanctuary == outputVD3$Sanctuary[ii] , ]
  # unique( tmpAB$Sanctuary )
  
  #find the closest (in time) ambient sample
  idx = which.min( abs(outputVD3$Start[ii] - tmpAB$Start) )
  signalMed = outputVD3$`TOL_125 median`[ii]
  noiseMed  = tmpAB$`TOL_125 median`[idx]
  
  outputVD3$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD3$SNRtime[ii] = as.numeric( difftime(outputVD3$Start[ii], tmpAB$Start[idx], units = "mins") )
  
  signalMed = outputVD3$`TOL_125 max`[ii]
  noiseMed  = tmpAB$`TOL_125 max`[idx]
  outputVD3$SNRmax[ii]= signalMed - noiseMed # signal - noise
  
}

# METRIC: mean and standard error max noise above closest non-vessel periods (for all vessel detection periods + AIS label)
VExcee_mu  = aggregate(outputVD3$SNRmax, by=list(outputVD3$Sanctuary, outputVD3$Mth), mean, na.rm=T)
VExceed_se = aggregate(outputVD3$SNRmax, by=list(outputVD3$Sanctuary, outputVD3$Mth), std.error, na.rm=T)
VD_n = as.data.frame( outputVD3 %>% group_by(Sanctuary, Mth) %>% tally() ) 

Vexceed = merge( VExcee_mu, VExceed_se, by = c("Group.1","Group.2"), all.y = FALSE) 
colnames(Vexceed ) = c("Sanctuary", "Mth","SNRmax_mean","SNRmax_se")
Vexceed = merge( Vexceed, VD_n, by = c("Sanctuary", "Mth") )
colnames(Vexceed ) = c("Sant", "mth","SNRmax_mean","SNRmax_se","VDs") 

# FIGURE 4: SEASONAL TRENDS 
# with 1-min TOLs
# y-axis METRIC= month, x = Exceedance
# SHAPE METRIC = site, color = dominance, size = AIS

VNP = merge( Vexceed, ND, by = c("Sant", "mth") )

VNPsb = VNP[VNP$Sant =="SB03",]
VNPsb$SNRmax_mean01 = range01(VNPsb$SNRmax_mean)*20
VNPsb$VDs01 = range01(VNPsb$VDs)*20

VNPgr = VNP[VNP$Sant =="GR01",]
VNPgr$SNRmax_mean01 = range01(VNPgr$SNRmax_mean)*20
VNPgr$VDs01 = range01(VNPgr$VDs)*20

VNP2 = rbind(VNPsb, VNPgr)
VNP2$Mth[VNP2$mth == 1] = "Jan"
VNP2$Mth[VNP2$mth == 2] = "Feb"
VNP2$Mth[VNP2$mth == 3] = "Mar"
VNP2$Mth[VNP2$mth == 4] = "Apr"
VNP2$Mth[VNP2$mth == 5] = "May"
VNP2$Mth[VNP2$mth == 6] = "Jun"
VNP2$Mth[VNP2$mth == 7] = "Jul"
VNP2$Mth[VNP2$mth == 8] = "Aug"
VNP2$Mth[VNP2$mth == 9] = "Sep"
VNP2$Mth[VNP2$mth == 10] = "Oct"
VNP2$Mth[VNP2$mth == 11] = "Nov"
VNP2$Mth[VNP2$mth == 12] = "Dec"

VNP2$Mth = factor( VNP2$Mth, levels =c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") )


ggplot(VNP2, aes(x = as.factor(Mth), perTime*100, color=SNRmax_mean, label = as.character( round_any(SNRmax_mean,.1) ) ) ) +
  geom_point(size = VNP2$VDs01+5 )  +  # relative number of vessel detections per site
  geom_text(hjust=.5, vjust=-2)+ #exceedence value
  scale_color_gradientn(colours = viridis(20))+
  theme_minimal()+
  xlab("")+  ylab("% of time with vessel noise") + ggtitle("") +
  labs(caption = "Bubble size = relative number of vessel detections", color = "Noise Exceedence" )+
  theme(  text = element_text(size =20) )

VNP2$Mth = factor( VNP2$Mth, levels =c("Dec","Nov","Oct","Sep","Aug","Jul","Jun","May","Apr","Mar","Feb","Jan") )
ggplot(VNP2, aes(y= as.factor(Mth), color= perTime*100, x=SNRmax_mean, label = round( perTime*100 ) ) )  +
  geom_point(size = VNP2$VDs01+5)  +  # relative number of vessel detections per site
  geom_text(hjust=0, vjust=3)+ #exceedence value
  #scale_colour_gradient(low = "gray", high = "black")+
  scale_color_gradientn(colours = viridis (10))+
  theme_minimal()+
  #facet_wrap(~Sant)+
  xlab("")+  ylab("") + ggtitle("") +
  labs(caption = "Bubble size = relative number of vessel detections", color = "% of time with vessel noise" )+
  theme(  text = element_text(size =20) )

# BY SITE WITH DIFFERENT SCALES
VR = VNP2[VNP2$Sant == "GR01",]
pGR = ggplot(VR, aes(y= as.factor(Mth), color= perTime*100, x=SNRmax_mean) )  +
  geom_point(size = VR$VDs01+3, shape=17)  +  # relative number of vessel detections per site
  scale_color_gradientn(colours = viridis (10)) +
  xlim(c(-20,0))+
  theme_minimal()+
  xlab("")+  ylab("") + ggtitle("") +
  labs(caption = "Triangle size = relative number of vessel detections", color = "% of time with vessel noise" )+
  theme( text = element_text(size =20) )
pGR

VR = VNP2[VNP2$Sant == "SB03",]
pSB = ggplot(VR, aes(y= as.factor(Mth), color= round( perTime*100), x=SNRmax_mean ) )  +
  geom_point(size = VR$VDs01+3, shape=19)  +  # relative number of vessel detections per site
  scale_color_gradientn(colours = viridis (10)) +
  xlim(c(0,5))+
  theme_minimal()+
  xlab("")+  ylab("") + ggtitle("") +
  labs(caption = "Circle size = relative number of vessel detections", color = "% of time with vessel noise" )+
  theme( text = element_text(size =20) )
pSB

grid.arrange(pGR, pSB, nrow = 1)

# FIGURE 5: MANDATORY SLOWDOWN ####
# head( TOL_SB03)
# head( VD_SB03 )
TOLmin2 = TOL_SB03[1:400,] #only plot first month
p = ggplot(TOLmin2, aes(x=DateF, y=Label, fill=(TOL_125)) ) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_bw() + 
  ggtitle("Vessel Detection Label")+
  scale_y_discrete(limits=rev)+ 
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(20))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )
ggplotly(p) 

#A: distributions of low frequency sound levels
# only during vessel detection periods
TOL_VD = TOL_SB03[TOL_SB03$Label == "A. Vessel Detection", ]
as.data.frame( TOL_VD %>% group_by(Slow) %>% tally() )

p1 = ggplot(TOL_VD, aes(TOL_125, color = Slow) ) +
  stat_ecdf(geom="step", size = 2) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels  (125 Hz third-octave band)") ) +
  theme_bw() +
  #labs(caption = "Mandatory slowdown March 1 - April 30, 2019")+
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )

#A: distributions of low frequency sound levels
# all sound levels!!
TOL_VD = TOL_SB03[TOL_SB03$Label == "A. Vessel Detection", ]
#as.data.frame( TOL_VD %>% group_by(Slow) %>% tally() )

p1 = ggplot(TOL_SB03, aes(TOL_125, color = Slow) ) +
  stat_ecdf(geom="step", size = 2) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels (125 Hz third-octave band)") ) +
  theme_bw() +
  #labs(caption = "Mandatory slowdown March 1 - April 30, 2019")+
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )



#B: % of time with vessel noise, relative to number of vessels present
# not created because in Figure 4

#C: Noise exceedance valuse by month
outputVD_SB = outputVD3[outputVD3$Sanctuary == "SB03",]

outputVD_SB$slow = "No"
outputVD_SB$slow [ outputVD_SB$Mth == 3 | outputVD_SB$Mth == 4] = "Slowdown"

p2 = ggplot(outputVD_SB, aes(SNRmax, color = as.factor( slow )  ) ) +
  stat_ecdf(geom="step", size = 2) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(noise exceedance)") +  xlab(paste0("Noise Exceedance (125 Hz third-octave band)") ) +
  theme_bw() +
  #labs(caption = "Mandatory slowdown March 1 - April 30, 2019")+
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )

unique(outputVD_SB$Category)
outputVD_SBais = outputVD_SB[outputVD_SB$Category == "A. AIS vessels" ,]

p3 = ggplot(outputVD_SBais, aes(SNRmax, color = as.factor( slow )  ) ) +
  stat_ecdf(geom="step", size = 2) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(noise exceedance)") +  xlab(paste0("Noise Exceedance (125 Hz third-octave band)") ) +
  theme_bw() +
  #labs(caption = "Mandatory slowdown March 1 - April 30, 2019")+
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )


grid.arrange(p1,p2,p3, nrow = 1)
 