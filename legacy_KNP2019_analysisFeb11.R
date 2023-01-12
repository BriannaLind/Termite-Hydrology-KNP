#######
### Reworking data for first time in a long time

#  line plot of individual mounds/ ped/ mat sequence in filitration change

# Set working directory
setwd("C:/Users/brian/Desktop/FieldWork_KNPFall2019/DATA/")
# Add Infiltration data
Data.infil <- read.csv("Infiltration_ALL1.csv")
Moist2 <- read.csv("Moist2.csv")
setwd("C:/Users/brian/Desktop/FieldWork_KNPFall2019/")
# Add data Compaction
Data.comp <- read.csv("CompactionTests1.csv")
Data.soilM <- read.csv("SoilMoisture.csv")

# Add neccessary libraries
if (!require(ggplot2)) install.packages('ggplot2') # installs package if not already installed
library(ggplot2)                                   # loads package
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
library(tidyr)
library(rstatix)

#Prepare Moisture Data
# Plot the data
# make sure Mound Components are plotted in desired order
Data.soilM$Component = factor(Data.soilM$Component, levels = c("Mound","Pediment","Matrix"))
Moist$Component = factor(Moist$Component, levels = c("Mound","Pediment","Matrix"))
Data.soilSum <- Data.soilM %>%
  group_by(Mound, Component) %>%
  get_summary_stats(Moisture, type = "mean_sd")

colnames(Data.soilSum)[5] <- "AvgMoist"
colnames(Data.soilSum)[6] <- "SDMoist"


ND <- Data.comp %>%
  group_by(Mound, Geology, Vegetation, Component) %>%
  summarise( avg = median(Strike10),
             std = sd(Strike10),
             n = n())
ND <- dplyr::mutate(ND, SE = std/(sqrt(n)))
A <- dplyr::left_join(Data.infil, ND)
Data.infil <- A

colnames(A)[9] <- "AvgDepth10S"
colnames(A)[11] <- "SE_avgd10s"

# subset data so can be combined with other
#Dsoil <- dplyr::filter(Data.soilM, Sample == "A")
Dsoil <- dplyr::select(Data.soilSum, Mound, Component, AvgMoist, SDMoist)
Moist <- dplyr::left_join(Data.infil, Dsoil, by = c("Mound", "Component"))
names(CompactMoist)[11] <- "Depthcm"
ICM.df <- dplyr::left_join(Data.infil, CompactMoist, by = c("Geology", "Mound", "Vegetation", "Component"))

# Prepare Compaction Data
DataLong  <- gather(Data.comp, Strikes, Value, Strike1:Strike10, factor_key=TRUE)

# add in cumulative enegery values
DataLong$Strikes <- as.factor(gsub("Strike1", 17.052, gsub("Strike2", 34.104, gsub("Strike3", 51.156,
                                                                                   gsub("Strike4", 68.208, gsub("Strike5", 85.26, gsub("Strike6", 102.312,
                                                                                                                                       gsub("Strike7", 119.364,gsub("Strike8", 136.416, gsub("Strike9", 153.468,
                                                                                                                                                                                             gsub("Strike10", 170.52, DataLong$Strikes)))))))))))
DataLong$Component = factor(DataLong$Component, levels = c("Mound","Pediment","Matrix"))
DataLong$Strikes = factor(DataLong$Strikes, levels = c("17.052","34.104","51.156","68.208","85.26","102.312","119.364","136.416","153.468","170.52"))

DataLong$Strikes <- as.numeric(DataLong$Strikes)
ggplot(data=DataLong, aes(x=Strikes, y=Value, color=Vegetation)) + geom_point()+
  facet_grid(.~Component)+ (geom_smooth(method="lm", aes(group=Vegetation)))+
  theme_bw()+ theme(text = element_text(size = 20))+ theme(legend.position = "top")+
  scale_color_manual(values=c("yellow2","seagreen3"))+             # Change colors
  #ylim(0, 40) + xlab("Cumulative Energy J/cm-2") + ylab("Penetrometer Depth (cm)")
  ylim(0, 40) + labs(x = expression(paste("Cumulative Energy Jcm "^"-2")), y = "Penetrometer Depth (cm)")

# See what the data looks like:
head(Data.infil)
Moist2$Component <- factor(Moist2$Component, levels = c("Mound", "Pediment", "Matrix"))
Moist2$Geology <- factor(Moist2$Geology, levels = c("Granite", "Shale", "Basalt"))

ggplot(data=Moist2,  aes(x=psandMatrix, y=HC_mmh, color=Vegetation)) + geom_point()+
  facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Infiltration (mm/h)")+scale_color_manual(values=c("yellow2","seagreen3"))+
  theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
ggplot(data=Moist2,  aes(x=psandMatrix, y=AvgMoist, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
  facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Average Percent Soil Moisture")+
  theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
ggplot(data=Moist2,  aes(x=psandMatrix, y=pclay, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
  facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Soil Percent Clay")+
  theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
ggplot(data=Moist2,  aes(x=psandMatrix, y=AvgDepth10S, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
  facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Depth after 10 Stikes (cm)")+
  theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")

# make components in the correct order:
Data.infil$Component <- as.factor(Data.infil$Component)
Data.infil$Geology <- as.factor(Data.infil$Geology)
levels(Data.infil$Component)
levels(Data.infil$Geology)
Data.infil$Component <- factor(Data.infil$Component, levels = c("Mound", "Pediment", "Matrix"))
Data.infil$Geology <- factor(Data.infil$Geology, levels = c("Granite", "Shale", "Basalt"))


# fix structure of data so can label correctly
go   <- dplyr::select(Moist2, Geology:Component, psandMatrix, HC_mmh:pclay, AvgMoist, AvgDepth3S)
go.l <- gather(go, Stat, Value, HC_mmh:AvgDepth3S)
go.l$Stat <- as.factor(go.l$Stat)
levels(go.l$Stat) <- c("Avg. Depth Strike 3 (cm)", "Avg. % Soil Moisture", "Infiltration (mm/h)", "Soil % Clay")
go.l$Stat <- factor(go.l$Stat, levels =c( "Infiltration (mm/h)", "Avg. Depth Strike 3 (cm)", "Avg. % Soil Moisture", "Soil % Clay"))
levels(go.l$Stat) <- c( "Infiltration (mm/h)", "Depth after 1 Strikes (cm)", "Avg. % Soil Moisture", "% Clay")
go.l$Component <- factor(go.l$Component, levels = c("Mound", "Pediment", "Matrix"))


ggplot(data=go.l,  aes(x=psandMatrix, y=Value, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
  facet_grid(Stat~Component, scales="free")+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil")+
  theme_bw()+ theme(text = element_text(size = 20), axis.title.y = element_blank(), legend.position = "top")




B <- dplyr::left_join(Moist, Data.infil)
Moist <- B
# Running experimental regressions......
# in process noticed that plots based not on Matrix sand like wanted - but just normal sand.....
# below adjusting that to make new data frame.

InfilMatrix <- dplyr::filter(Data.infil, Component == "Matrix")
colnames(InfilMatrix)[8] <- "psandMatrix"
psandMat <- dplyr::select(InfilMatrix, Mound, psandMatrix)
Moist2 <- dplyr::left_join(Moist, psandMat, by = c("Mound"))

# Separate into bare and grassy to facilitate contrasts
Grass <- dplyr::filter(Moist2, Vegetation == "Grassy")
Bare <- dplyr::filter(Moist2, Vegetation == "Bare")

#Maybe do an ANCOVA  - who the F knows.


write.csv(Moist2, "Moist2.csv", row.names=FALSE)

library(car)
model.infil_1b = lm (HC_mmh ~ psandMatrix + Component, data = Bare)
Anova(model.infil_1b, type="II") # signif difference between bare mound compoenents infil (ped==mound, matrix is diff)

model.infil_1g = lm (HC_mmh ~ psandMatrix + Component, data = Grass)
Anova(model.infil_1g, type="II") # no difference between grassy mound components in infiltration
###########################
# This deals with "within grass" or "within bare" contrasts but not BETWEEN
Mound <- dplyr::filter(Moist2, Component == "Mound")
Pedim <- dplyr::filter(Moist2, Component == "Pediment")

model.infil_2m = lm (HC_mmh ~ psandMatrix + Vegetation, data = Mound)
Anova(model.infil_2m, type="II") # signif difference between bare and grassy mound infil

model.infil_2p = lm (HC_mmh ~ psandMatrix + Vegetation, data = Pedim)
Anova(model.infil_2p, type="II") # signif difference between bare and grassy ped infil

#######
model.moist_1b = lm (AvgMoist ~ psandMatrix + Component, data = Bare)
Anova(model.moist_1b, type="II")# signif difference between bare mound compoenents infil (ped==mound, matrix is diff)

model.moist_2g = lm (AvgMoist ~ psandMatrix + Component, data = Grass)
Anova(model.moist_2g, type="II")  # no difference between grassy mound components in moisture

model.moist_2m = lm (AvgMoist ~ psandMatrix + Vegetation, data = Mound)
Anova(model.moist_2m, type="II") # signif difference between bare and grassy mound moisture

model.moist_2p = lm (AvgMoist ~ psandMatrix + Vegetation, data = Pedim)
Anova(model.moist_2p, type="II") # signif difference between bare and grassy ped infil

########
model.pclay_1b = lm (pclay ~ psandMatrix + Component, data = Bare)
Anova(model.pclay_1b, type="II")# mound has signif more clay than matrix

model.pclay_2g = lm (pclay ~ psandMatrix + Component, data = Grass)
Anova(model.pclay_2g, type="II")  # all different

model.pclay_2m = lm (pclay ~ psandMatrix + Vegetation, data = Mound)
Anova(model.pclay_2m, type="II") # NO signif difference between bare and grassy mound texture

model.pclay_2p = lm (pclay ~ psandMatrix + Vegetation, data = Pedim)
Anova(model.pclay_2p, type="II") # NO signif difference between bare and grassy ped texture



########
model.AD_1b = lm (AvgDepth1S ~ psandMatrix + Component, data = Bare)
Anova(model.AD_1b, type="II")# mound has signif dd than than matrix

model.AD_2g = lm (AvgDepth1S ~ psandMatrix + Component, data = Grass)
Anova(model.AD_2g, type="II")  # all different

model.AD_2m = lm (AvgDepth1S ~ psandMatrix + Vegetation, data = Mound)
Anova(model.AD_2m, type="II") # NO signif difference between bare and grassy mound texture

model.AD_2p = lm (AvgDepth1S ~ psandMatrix + Vegetation, data = Pedim)
Anova(model.AD_2p, type="II") # NO signif difference between bare and grassy ped texture




















# This is not appropriate because the strikes are not indepenedt  and cant be regressed
DataLong$Strikes <- as.numeric(DataLong$Strikes)
#####
DL_Grass <- dplyr::filter(DataLong, Vegetation == "Grassy")
DL_Bare <- dplyr::filter(DataLong, Vegetation == "Bare")
DL_Mound <- dplyr::filter(DataLong, Component == "Mound")
DL_Pedim <- dplyr::filter(DataLong, Component == "Pediment")

model.strike_1b = lm (Value ~ Strikes + Component, data = DL_Bare)
Anova(model.strike_1b, type="II")# all different

model.strike_2g = lm (Value ~ Strikes + Component, data = DL_Grass)
Anova(model.strike_2g, type="II")  # all different

model.strike_2m = lm (Value ~ Strikes + Vegetation, data = DL_Mound)
Anova(model.strike_2m, type="II") # signif difference between bare and grassy mound strikes

model.strike_2p = lm (Value ~ Strikes + Vegetation, data = DL_Pedim)
Anova(model.strike_2p, type="II") # signif difference between bare and grassy ped texture

yikes4 = lm (Value ~ Strikes + Vegetation + Component +Strikes:Vegetation +
               Strikes:Component +  Component:Vegetation +
               Strikes:Vegetation:Component, data = DataLong)


















otters.model <- lm(Otters ~ Location + Year + Location:Year, data = seaotters)


model.infil_1b = lm (HC_mmh ~ psandMatrix + Component, data = Bare)
Anova(model.infil_1b, type="II") # signif difference between bare mound compoenents infil (ped==mound, matrix is diff)

model.infil_1g = lm (HC_mmh ~ psandMatrix + Component, data = Grass)
Anova(model.infil_1g, type="II") # no difference between grassy mound components in infiltration
###########################
# This deals with "within grass" or "within bare" contrasts but not BETWEEN
Mound <- dplyr::filter(Moist2, Component == "Mound")
Pedim <- dplyr::filter(Moist2, Component == "Pediment")

model.infil_2m = lm (HC_mmh ~ psandMatrix + Vegetation +psandMatrix:Vegetation, data = Moist2)
plot(model.infil_2m, add.smooth = FALSE, which = 1)
plot(model.infil_2m, which = 2)
plot(model.infil_2m, add.smooth = FALSE, which = 3)
anova(model.infil_2m) # signif difference between bare and grassy mound infil

model.infil_2p = lm (HC_mmh ~ psandMatrix + Vegetation, data = Pedim)
Anova(model.infil_2p, type="II") # signif difference between bare and grassy ped infil


yikes = lm (Depthcm ~ psandMatrix + Vegetation + Component +psandMatrix:Vegetation +
              psandMatrix:Component +  Component:Vegetation +
              psandMatrix:Vegetation:Component, data = ICM.df)


yikes = lm (HC_mmh ~ psandMatrix + Vegetation + Component +psandMatrix:Vegetation +
              psandMatrix:Component +  Component:Vegetation +
              psandMatrix:Vegetation:Component, data = Moist2)


yikes2 = lm (AvgMoist ~ psandMatrix + Vegetation + Component +psandMatrix:Vegetation +
               psandMatrix:Component +  Component:Vegetation +
               psandMatrix:Vegetation:Component, data = Moist2)

yikes3 = lm (pclay ~ psandMatrix + Vegetation + Component +psandMatrix:Vegetation +
               psandMatrix:Component +  Component:Vegetation +
               psandMatrix:Vegetation:Component, data = Moist2)


other  <- lm(formula = AvgDepth1S ~ psandMatrix + Vegetation + Component +
               psandMatrix:Vegetation + psandMatrix:Component + Component:Vegetation +
               psandMatrix:Vegetation:Component, data = Moist2)

anova(yikes)
anova(other)
plot(model.infil_2m, add.smooth = FALSE, which = 1)
plot(model.infil_2m, which = 2)
plot(model.infil_2m, add.smooth = FALSE, which = 3)



ND <- Data.comp %>%
  group_by(Mound, Geology, Vegetation, Component) %>%
  summarise( avg = median(Strike10),
             std = sd(Strike10),
             n = n())
ND <- dplyr::mutate(ND, SE = std/(sqrt(n)))
A <- dplyr::left_join(Data.infil, ND)
Data.infil <- A

colnames(A)[9] <- "AvgDepth10S"
colnames(A)[11] <- "SE_avgd10s"

ND <- Data.comp %>%
  group_by(Mound, Geology, Vegetation, Component) %>%
  summarise( avg = median(Strike1),
             std = sd(Strike3),
             n = n())
ND <- dplyr::mutate(ND, SE = std/(sqrt(n)))
colnames(ND)[5] <- "AvgDepth1S"
colnames(ND)[8] <- "SE_avgds1"

A <- dplyr::left_join(Data.infil, ND)
Data.infil <- A


anova(model.infil_2m) # signif difference between bare and grassy mound infil
