# This is code to replicate the analysis and figures from (2023) "Termite Mound Impacts on Hydrology vary
# with Herbaceous Vegetation and Topsoil Texture." This code was developed by Bri Lind.

# Last updated 01/12/2023

# Requirements
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstatix)
library(car)
library(emmeans)


####### Data Integration #########################################################################
# Integrate data from 3 separate collection sheets so can analyse together
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





####### Make Plots###########################################################################################
  # Make plots/ Paper Figures  # Load PRE-PROCESSED DATA

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
        levels(go.l$Stat) <- c("Penentration Depth (cm)", "Avg. % Soil Moisture", "Infiltration (mm/h)", "Soil % Clay")
        go.l$Stat <- factor(go.l$Stat, levels =c( "Infiltration (mm/h)", "Penentration Depth (cm)", "Avg. % Soil Moisture", "Soil % Clay"))
        levels(go.l$Stat) <- c( "Infiltration (mm/h)", "Penentration Depth (cm)", "Avg. % Soil Moisture", "% Clay")
        go.l$Component <- factor(go.l$Component, levels = c("Mound", "Pediment", "Matrix"))

        ggplot(data=go.l,  aes(x=psandMatrix, y=Value, color=Vegetation)) + geom_point()+ scale_color_manual(values=c("yellow2","seagreen3"))+
          facet_grid(Stat~Component, scales="free")+(geom_smooth(method=lm))+labs(color = "Vegetation Cover", x = "Topsoil Texture: % Sand")+
          theme_bw()+ theme(text = element_text(size = 20), axis.title.y = element_blank(), legend.position = "top")


        rm(go, go.l)


####### Statistical Analyses ##########################################################################################
# Perform Statistical Analyses

# Use ANCOVA for analysis - example here: https://www.datanovia.com/en/lessons/ancova-in-r/
                              # and here: https://rcompanion.org/rcompanion/e_04.html
      # Want to compare means -  broadly similar assumption to ANOVA
      # ANCOVAs: generally used to refine estimates of experimental error and adjust for treatment effects.
      # there are 5 assumptions (Linearity, homogeneity of regression slopes, normality of residuals, homogeneity of variances, and no outliers).


# Subset Data to strategically perform contrasts ANCOVAS
      head(Moist2)

      # Preffered way
      a_Mou <- dplyr::filter(Moist2, Component == "Mound")
      b_Ped <- dplyr::filter(Moist2, Component == "Pediment")
      c_Mat <- dplyr::filter(Moist2, Component == "Matrix")

# For each of my 4 response variables (Infiltration, Depth after 1 strike, Avg. Soil Moisture, % Clay)
# Want to run an ANCOVA comparing grassy vs bare vegetation cover

 # # Response variable 1: INFILTRATION RATE (mm/h)
    # Mound, Infiltration
          # model.I1a = lm (HC_mmh ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = a_Mou) # model 1
          # Anova(model.I1a, type="II"): : no interaction effect found make new model
        model.I1b = lm (HC_mmh ~ psandMatrix + Vegetation,           data = a_Mou) # model 2
        Anova(model.I1b, type="II")
        summary(model.I1b)
        emmeans_test(a_Mou, HC_mmh ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

    # Pediment, Infiltration
          # model.I2a = lm (HC_mmh ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = b_Ped)
          # Anova(model.I2a, type="II"): no interaction effect found make new model
        model.I2b = lm (HC_mmh ~ psandMatrix + Vegetation,                          data = b_Ped)
        Anova(model.I2b, type="II")
        summary(model.I2b)
        emmeans_test(b_Ped, HC_mmh ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

    # Matrix, Infiltration
          # model.I3a = lm (HC_mmh ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = c_Mat)
          # Anova(model.I3a, type="II"): no interaction effect found make new model
        model.I3b = lm (HC_mmh ~ psandMatrix + Vegetation,                          data = c_Mat)
        Anova(model.I3b, type="II")
        summary(model.I3b)
        emmeans_test(c_Mat, HC_mmh ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")


 # Response variable 2: PENETRATION DEPTH (CM)
      # Mound, Penetration Depth
          # model.D1a = lm (AvgDepth1S ~ psandMatrix + Vegetation +psandMatrix*Vegetation, data = a_Mou)
          # Anova(model.D1a, type="II"): no interaction effect found make new model
        model.D1b = lm (AvgDepth1S ~ psandMatrix + Vegetation, data = a_Mou)
        Anova(model.D1b, type="II")
        summary(model.D1b)
        emmeans_test(a_Mou, AvgDepth1S ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

      # Pediment, Penetration Depth
           # model.D2a = lm (AvgDepth1S ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = b_Ped)
           # Anova(model.D2a, type="II"): no interaction effect found make new model
        model.D2b = lm (AvgDepth1S ~ psandMatrix + Vegetation , data = b_Ped)
        Anova(model.D2b, type="II")
        summary(model.D2b)

      # Matrix, Penetration depth
            # model.D3a = lm (AvgDepth1S ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = c_Mat)
            # Anova(model.D3a, type="II"): no interaction effect found make new model
        model.D3b = lm (AvgDepth1S ~ psandMatrix + Vegetation, data = c_Mat)
        Anova(model.D3b, type="II")
        summary(model.D3b)



 # Response variable 3: % SOIL MOISTURE
        # Mound, Moisture
        model.M1a = lm (AvgMoist ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = a_Mou)
        Anova(model.M1a, type="II")
        summary(model.M1a)
        emmeans_test(a_Mou, AvgMoist ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

        # Pediment, Moisture
            # model.M2a = lm (AvgMoist ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = b_Ped)
            # Anova(model.M2a, type="II"): no interaction effect found make new model
        model.M2b = lm (AvgMoist ~ psandMatrix + Vegetation, data = b_Ped)
        Anova(model.M2b, type="II")
        summary(model.M2b)
        emmeans_test(b_Ped, AvgMoist ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

        # Matrix, Moisture
            # model.M3a = lm (AvgMoist ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = c_Mat)
            # Anova(model.M3a, type="II"): no interaction effect found make new model
        model.M3b = lm (AvgMoist ~ psandMatrix + Vegetation, data = c_Mat)
        Anova(model.M3b, type="II")
        summary(model.M3b)


 # Response variable 4: % SOIL CLAY
        # Mound, Clay
        model.C1a = lm (pclay ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = a_Mou)
        Anova(model.C1a, type="II")#: no interaction effect found make new model
        model.C1b = lm (pclay ~ psandMatrix + Vegetation, data = a_Mou)
        Anova(model.C1b, type="II")
        summary(model.C1b)
        emmeans_test(a_Mou, pclay ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

        # Pediment, Clay
            # model.C2a = lm (pclay ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = b_Ped)
            # Anova(model.C2a, type="II"): no interaction effect found make new model
        model.C2b = lm (pclay ~ psandMatrix + Vegetation, data = b_Ped)
        Anova(model.C2b, type="II")
        summary(model.C2b)
        emmeans_test(b_Ped, pclay ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")

        # Matrix, Clay
            # model.C3a = lm (pclay ~ psandMatrix + Vegetation + psandMatrix*Vegetation, data = c_Mat)
            # Anova(model.C3a, type="II"): no interaction effect found make new model
        model.C3b = lm (pclay ~ psandMatrix + Vegetation, data = c_Mat)
        Anova(model.C3b, type="II")
        summary(model.C3b)
        emmeans_test(c_Mat, pclay ~ Vegetation, covariate = psandMatrix, p.adjust.method = "bonferroni")






##### ALT STATS #############################################################################################
      # Just to double check way
      d_G   <- dplyr::filter(Moist2, Vegetation == "Grassy")
      e_B   <- dplyr::filter(Moist2, Vegetation == "Bare")

 # Response variable 1: INFILTRATION RATE (mm/h)
      # Grassy, Infiltration
          # model.GIa = lm (HC_mmh ~ psandMatrix + Component + psandMatrix*Component, data = d_G) # model 1
          # Anova(model.GIa, type="II"): no interaction effect found make new model
      model.GIb = lm (HC_mmh ~ psandMatrix + Component,           data = d_G) # model 2
      Anova(model.GIb, type="II")
      summary(model.GIb)
      emmeans_test(d_G, HC_mmh ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

      # Bare, Infiltration
      model.BIa = lm (HC_mmh ~ psandMatrix + Component + psandMatrix*Component, data = e_B) # model 1
      Anova(model.BIa, type="II")
      summary(model.BIa)
      emmeans_test(e_B, HC_mmh ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

 # Response variable 2: PENETRATION DEPTH
      # Grassy, Penetration
          # model.GDa = lm (AvgDepth1S ~ psandMatrix + Component + psandMatrix*Component, data = d_G) # model 1
          # Anova(model.GDa, type="II")#: no interaction effect found make new model
      model.GDb = lm (AvgDepth1S ~ psandMatrix + Component,           data = d_G) # model 2
      Anova(model.GDb, type="II")
      summary(model.GDb)
      emmeans_test(d_G, AvgDepth1S ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

      # Bare, Penetration
          # model.BDa = lm (AvgDepth1S ~ psandMatrix + Component + psandMatrix*Component, data = e_B) # model 1
          # Anova(model.BDa, type="II")
      model.BDb = lm (AvgDepth1S ~ psandMatrix + Component,           data = e_B) # model 2
      Anova(model.BDb, type="II")
      summary(model.BDb)
      emmeans_test(e_B, AvgDepth1S ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

 # Response variable 3: SOIL MOISTURE
      # Grassy, moisture
          # model.GMa = lm (AvgMoist ~ psandMatrix + Component + psandMatrix*Component, data = d_G) # model 1
          # Anova(model.GMa, type="II")#: no interaction effect found make new model
      model.GMb = lm (AvgMoist ~ psandMatrix + Component,           data = d_G) # model 2
      Anova(model.GMb, type="II")
      summary(model.GMb)
      emmeans_test(d_G, AvgMoist ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

      # Bare, moisture
      model.BMa = lm (AvgMoist ~ psandMatrix + Component + psandMatrix*Component, data = e_B) # model 1
      Anova(model.BMa, type="II")
      summary(model.BMa)
      emmeans_test(e_B, AvgMoist ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

 # Response variable 4: PERENT CLAY
      # Grassy, clay
          # model.GCa = lm (pclay ~ psandMatrix + Component + psandMatrix*Component, data = d_G) # model 1
          # Anova(model.GCa, type="II")#: no interaction effect found make new model
      model.GCb = lm (pclay ~ psandMatrix + Component,           data = d_G) # model 2
      Anova(model.GCb, type="II")
      summary(model.GCb)
      emmeans_test(d_G, pclay ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

      # Bare, moisture
          # model.BCa = lm (pclay ~ psandMatrix + Component + psandMatrix*Component, data = e_B) # model 1
          # Anova(model.BCa, type="II")
      model.BCb = lm (pclay ~ psandMatrix + Component + psandMatrix, data = e_B) # model 1
      Anova(model.BCb, type="II")
      summary(model.BCb)
      emmeans_test(e_B, pclay ~ Component, covariate = psandMatrix, p.adjust.method = "bonferroni")

################################################################################################################################################
    #### Supplemental Figures
              # tO Plot all data simultaneously IN 3X3X4 PLOT
              # fix structure of data so can label correctly
              go   <- dplyr::select(Moist2, Geology:Component, psandMatrix, HC_mmh:pclay, AvgMoist, AvgDepth1S)
              go.l <- gather(go, Stat, Value, HC_mmh:AvgDepth1S)
              go.l$Stat <- as.factor(go.l$Stat)
              levels(go.l$Stat) <- c("Penentration Depth (cm)", "Avg. % Soil Moisture", "Infiltration (mm/h)", "Soil % Clay")
              go.l$Stat <- factor(go.l$Stat, levels =c( "Infiltration (mm/h)", "Penentration Depth (cm)", "Avg. % Soil Moisture", "Soil % Clay"))
              levels(go.l$Stat) <- c( "Infiltration (mm/h)", "Penentration Depth (cm)", "Avg. % Soil Moisture", "% Clay")
              go.l$Component <- factor(go.l$Component, levels = c("Mound", "Pediment", "Matrix"))

              ggplot(data=go.l,  aes(x=psandMatrix, y=Value, color=Component)) + geom_point()+ scale_color_manual(values=c("black","orange", "green"))+
                facet_grid(Stat~Vegetation, scales="free")+(geom_smooth(method=lm))+labs(color = "r", x = "Topsoil Texture: % Sand")+
                theme_bw()+ theme(text = element_text(size = 20), axis.title.y = element_blank(), legend.position = "top")
