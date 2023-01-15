# This is code to replicate the analysis and figures from (2023) "Termite Mound Impacts on Hydrology vary
# with Herbaceous Vegetation and Topsoil Texture." This code was developed by Bri Lind.

# Last updated 01/12/2023

# Requirements
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)

# Integrate data from 3 separate collection sheets so can analyse together
##################################################################################################
# Load RAW DATA (from desktop for now)
  setwd("C:/Users/brian/Desktop/FieldWork_KNPFall2019/DATA/")
  Data.infil <- read.csv("Infiltration_ALL1.csv") # Infiltration data

  setwd("C:/Users/brian/Desktop/FieldWork_KNPFall2019/")
  Data.comp <- read.csv("CompactionTests1.csv")   # Compaction
  Data.soilM <- read.csv("SoilMoisture.csv")      # Soil Moisture

# Manipulate data frames to combine data in one spot
  # Combine separate data tabes for analysis:
  # To ADD COMPACTION DATA to Infiltration data
  ND <- Data.comp %>%
    group_by(Mound, Geology, Vegetation, Component) %>%
    summarise( avg = median(Strike1),
               std = sd(Strike3),
               n = n())
  ND <- dplyr::mutate(ND, SE = std/(sqrt(n)))
  colnames(ND)[5] <- "AvgDepth1S"
  colnames(ND)[8] <- "SE_avgds1"
  Data.infil <- dplyr::left_join(Data.infil, ND)

  # To ADD Percent Sand Matrix to Infiltration data
  # Make matrix only column
  InfilMatrix <- dplyr::filter(Data.infil, Component == "Matrix")
  # Rename percent sand from matrix data at each mound to represent overall matrix
  colnames(InfilMatrix)[8] <- "psandMatrix"
  # Select only data of interest, drop columns that are extra
  psandMat <- dplyr::select(InfilMatrix, Mound, psandMatrix)

  # Deal with moisture data * Mound is only one vegetation cover so do not need to include vegetation as a factor
  Data.soilM$Component = factor(Data.soilM$Component, levels = c("Mound","Pediment","Matrix"))
  Data.soilSum <- Data.soilM %>%
    group_by(Mound, Component) %>%
    get_summary_stats(Moisture, type = "mean_sd")
  colnames(Data.soilSum)[5] <- "AvgMoist"
  colnames(Data.soilSum)[6] <- "SDMoist"

  Dsoil <- dplyr::select(Data.soilSum, Mound, Component, AvgMoist, SDMoist)
  Moist <- dplyr::left_join(Data.infil, Dsoil, by = c("Mound", "Component"))

  # To ADD Percent Sand Matrix to Infiltration data
  # Combine dataframes so everything is in the right spot
  Moist2 <- dplyr::left_join(Moist, psandMat, by = c("Mound"))

  # Remove Unneccessary Variables
  rm(Data.comp, Data.infil, Data.soilM, Data.soilSum, Dsoil, InfilMatrix, Moist, ND, psandMat)

  # Option to export CSV file
  write.csv(Moist2, "Moist2.csv", row.names=FALSE)


  # Work with Pre-processed data

# Make plots/ Paper Figures
##################################################################################################

  # Load PRE-PROCESSED DATA
  setwd("C:/Users/brian/Desktop/FieldWork_KNPFall2019/DATA/")
  Moist2 <- read.csv("Moist2.csv")
  head(Moist2)

  # Assign factors so will plot correctly
  Moist2$Component <- factor(Moist2$Component, levels = c("Mound", "Pediment", "Matrix"))
  Moist2$Geology <- factor(Moist2$Geology, levels = c("Granite", "Shale", "Basalt"))

        # Makes 4 separate plots, see change in y = for variable
        ggplot(data=Moist2,  aes(x=psandMatrix, y=HC_mmh, color=Vegetation)) + geom_point()+
          facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Infiltration (mm/h)")+scale_color_manual(values=c("yellow2","seagreen3"))+
          theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
        ggplot(data=Moist2,  aes(x=psandMatrix, y=AvgMoist, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
          facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Average Percent Soil Moisture")+
          theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
        ggplot(data=Moist2,  aes(x=psandMatrix, y=pclay, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
          facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Soil Percent Clay")+
          theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")
        ggplot(data=Moist2,  aes(x=psandMatrix, y=AvgDepth1S, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
          facet_grid(.~Component)+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil", y = "Depth after 1 Stike (cm)")+
          theme_bw()+ theme(text = element_text(size = 20), legend.position = "top")


    # tO Plot all data simultaneously IN 3X3X4 PLOT
    # fix structure of data so can label correctly
        go   <- dplyr::select(Moist2, Geology:Component, psandMatrix, HC_mmh:pclay, AvgMoist, AvgDepth1S)
        go.l <- gather(go, Stat, Value, HC_mmh:AvgDepth1S)
        go.l$Stat <- as.factor(go.l$Stat)
        levels(go.l$Stat) <- c("Avg. Depth Strike 1 (cm)", "Avg. % Soil Moisture", "Infiltration (mm/h)", "Soil % Clay")
        go.l$Stat <- factor(go.l$Stat, levels =c( "Infiltration (mm/h)", "Avg. Depth Strike 3 (cm)", "Avg. % Soil Moisture", "Soil % Clay"))
        levels(go.l$Stat) <- c( "Infiltration (mm/h)", "Depth after 1 Strikes (cm)", "Avg. % Soil Moisture", "% Clay")
        go.l$Component <- factor(go.l$Component, levels = c("Mound", "Pediment", "Matrix"))

        ggplot(data=go.l,  aes(x=psandMatrix, y=Value, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
          facet_grid(Stat~Component, scales="free")+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "% Sand Matrix Soil")+
          theme_bw()+ theme(text = element_text(size = 20), axis.title.y = element_blank(), legend.position = "top")


# Perform Statistical Analyses
##################################################################################################



# Separate into bare and grassy to facilitate contrasts
Grass <- dplyr::filter(Moist2, Vegetation == "Grassy")
Bare <- dplyr::filter(Moist2, Vegetation == "Bare")

#Maybe do an ANCOVA  - who the F knows.




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
  anova(other)
  plot(model.infil_2m, add.smooth = FALSE, which = 1)
  plot(model.infil_2m, which = 2)
  plot(model.infil_2m, add.smooth = FALSE, which = 3)

  anova(model.infil_2m) # signif difference between bare and grassy mound infil





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
