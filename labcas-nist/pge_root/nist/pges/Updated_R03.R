# FIXME
#setwd("/usr/local/labcas/staging/Lab005_C_R03")

SP_R03 <- "Lab005_A"  ### Specify Site_Platform for BAplots

### Set Consensus factors
# Bf <- 0.348
# Lf <- 0.167
# Pf <- 0.485
Bf <- 1
Lf <- 1
Pf <- 1

### Load libraries
# library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library(reshape)
library(ggplot2)
library(plyr)

### Import files by Round#
Sample_Key_R03<- read.delim("R03_Key.txt")
files <- list.files(pattern="*R03.txt")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.delim(paste(file,sep=""),colClasses=c("character",rep("numeric",15))))
}

### Import annotation files
miRBase20_mature_wHC <- read.delim("miRbase20_human_mature_wHC.txt")

### DATA NORMALIZATION
## NanoString normalizations (B=PosCtrl, C=T100, and D=PC_T100)
Lab005_B_R03 <- merge(miRBase20_mature_wHC, Lab005_B_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab005_B_R03[8:22], na.rm=TRUE) >= 50,
       Lab005_B_R03 <- cbind(Lab005_B_R03[1:7], log2(Lab005_B_R03[8:22])),
       Lab005_B_R03 <- cbind(Lab005_B_R03[1:7], (-1)*Lab005_B_R03[8:22]))
Lab005_B_R03$Site_Platform <- "Lab005_B"

Lab005_C_R03 <- merge(miRBase20_mature_wHC, Lab005_C_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab005_C_R03[8:22], na.rm=TRUE) >= 50,
       Lab005_C_R03 <- cbind(Lab005_C_R03[1:7], log2(Lab005_C_R03[8:22])),
       Lab005_C_R03 <- cbind(Lab005_C_R03[1:7], (-1)*Lab005_C_R03[8:22]))
Lab005_C_R03$Site_Platform <- "Lab005_C"

Lab005_D_R03 <- merge(miRBase20_mature_wHC, Lab005_D_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab005_D_R03[8:22], na.rm=TRUE) >= 50,
       Lab005_D_R03 <- cbind(Lab005_D_R03[1:7], log2(Lab005_D_R03[8:22])),
       Lab005_D_R03 <- cbind(Lab005_D_R03[1:7], (-1)*Lab005_D_R03[8:22]))
Lab005_D_R03$Site_Platform <- "Lab005_D"

### Library Size Normalize NGS data
Lab006_A_R03_colSums <- colSums(Lab006_A_R03[2:16], na.rm = TRUE, dims = 1)
Lab006_A_max <- median(Lab006_A_R03_colSums)
Lab006_A_factors <- Lab006_A_max/Lab006_A_R03_colSums
for(i in 1:length(Lab006_A_factors)){
  Lab006_A_R03[i+1]<-Lab006_A_R03[i+1]*Lab006_A_factors[i]
}

Lab006_B_R03_colSums <- colSums(Lab006_B_R03[2:16], na.rm = TRUE, dims = 1)
Lab006_B_max <- median(Lab006_B_R03_colSums)
Lab006_B_factors <- Lab006_B_max/Lab006_B_R03_colSums
for(i in 1:length(Lab006_B_factors)){
  Lab006_B_R03[i+1]<-Lab006_B_R03[i+1]*Lab006_B_factors[i]
}

Lab007_A_R03_colSums <- colSums(Lab007_A_R03[2:16], na.rm = TRUE, dims = 1)
Lab007_A_max <- median(Lab007_A_R03_colSums)
Lab007_A_factors <- Lab007_A_max/Lab007_A_R03_colSums
for(i in 1:length(Lab007_A_factors)){
  Lab007_A_R03[i+1]<-Lab007_A_R03[i+1]*Lab007_A_factors[i]
}

Lab008_A_R03_colSums <- colSums(Lab008_A_R03[2:16], na.rm = TRUE, dims = 1)
Lab008_A_max <- median(Lab008_A_R03_colSums)
Lab008_A_factors <- Lab008_A_max/Lab008_A_R03_colSums
for(i in 1:length(Lab008_A_factors)){
  Lab008_A_R03[i+1]<-Lab008_A_R03[i+1]*Lab008_A_factors[i]
}

### Log2 Transform and Annotate
# [8:16] changed to [8:22] to accommodate 6 more samples
# Annotate files and check for log2 data; test for log2 vs linear data (i.e. Ct vs. count data)

Lab001_A_R03 <- merge(miRBase20_mature_wHC, Lab001_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab001_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab001_A_R03 <- cbind(Lab001_A_R03[1:7], log2(Lab001_A_R03[8:22])),
       Lab001_A_R03 <- cbind(Lab001_A_R03[1:7], (-1)*Lab001_A_R03[8:22]))
Lab001_A_R03$Site_Platform <- "Lab001_A"

Lab002_A_R03 <- merge(miRBase20_mature_wHC, Lab002_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab002_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab002_A_R03 <- cbind(Lab002_A_R03[1:7], log2(Lab002_A_R03[8:22])),
       Lab002_A_R03 <- cbind(Lab002_A_R03[1:7], (-1)*Lab002_A_R03[8:22]))
Lab002_A_R03$Site_Platform <- "Lab002_A"

Lab003_A_R03 <- merge(miRBase20_mature_wHC, Lab003_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab003_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab003_A_R03 <- cbind(Lab003_A_R03[1:7], log2(Lab003_A_R03[8:22])),
       Lab003_A_R03 <- cbind(Lab003_A_R03[1:7], (-1)*Lab003_A_R03[8:22]))
Lab003_A_R03$Site_Platform <- "Lab003_A"

Lab004_A_R03 <- merge(miRBase20_mature_wHC, Lab004_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab004_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab004_A_R03 <- cbind(Lab004_A_R03[1:7], log2(Lab004_A_R03[8:22])),
       Lab004_A_R03 <- cbind(Lab004_A_R03[1:7], (-1)*Lab004_A_R03[8:22]))
Lab004_A_R03$Site_Platform <- "Lab004_A"

Lab005_A_R03 <- merge(miRBase20_mature_wHC, Lab005_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab005_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab005_A_R03 <- cbind(Lab005_A_R03[1:7], log2(Lab005_A_R03[8:22])),
       Lab005_A_R03 <- cbind(Lab005_A_R03[1:7], (-1)*Lab005_A_R03[8:22]))
Lab005_A_R03$Site_Platform <- "Lab005_A"

Lab006_A_R03 <- merge(miRBase20_mature_wHC, Lab006_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab006_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab006_A_R03 <- cbind(Lab006_A_R03[1:7], log2(Lab006_A_R03[8:22])),
       Lab006_A_R03 <- cbind(Lab006_A_R03[1:7], (-1)*Lab006_A_R03[8:22]))
Lab006_A_R03$Site_Platform <- "Lab006_A"

Lab006_B_R03 <- merge(miRBase20_mature_wHC, Lab006_B_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab006_B_R03[8:22], na.rm=TRUE) >= 50,
       Lab006_B_R03 <- cbind(Lab006_B_R03[1:7], log2(Lab006_B_R03[8:22])),
       Lab006_B_R03 <- cbind(Lab006_B_R03[1:7], (-1)*Lab006_B_R03[8:22]))
Lab006_B_R03$Site_Platform <- "Lab006_B"

Lab007_A_R03 <- merge(miRBase20_mature_wHC, Lab007_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab007_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab007_A_R03 <- cbind(Lab007_A_R03[1:7], log2(Lab007_A_R03[8:22])),
       Lab007_A_R03 <- cbind(Lab007_A_R03[1:7], (-1)*Lab007_A_R03[8:22]))
Lab007_A_R03$Site_Platform <- "Lab007_A"

Lab008_A_R03 <- merge(miRBase20_mature_wHC, Lab008_A_R03, by="Mature_Name", all.x=TRUE)
ifelse(max(Lab008_A_R03[8:22], na.rm=TRUE) >= 50,
       Lab008_A_R03 <- cbind(Lab008_A_R03[1:7], log2(Lab008_A_R03[8:22])),
       Lab008_A_R03 <- cbind(Lab008_A_R03[1:7], (-1)*Lab008_A_R03[8:22]))
Lab008_A_R03$Site_Platform <- "Lab008_A"

### Combine R03 datasets
R03_All_Wide <- rbind(Lab001_A_R03,
                      Lab003_A_R03,
                      Lab004_A_R03,
                      Lab005_A_R03,
                      Lab005_B_R03,
                      Lab005_C_R03,
                      Lab005_D_R03,
                      Lab006_A_R03,
                      Lab006_B_R03,
                      Lab007_A_R03,
                      Lab008_A_R03) 

R03_All_Tall <- melt(R03_All_Wide)
R03_All_Tall <- rename(R03_All_Tall, c(variable="Sample",value="Log2signal"))
R03_All_Tall <- merge(R03_All_Tall, Sample_Key_R03, by="Sample")

### Average Replicates Function ###
AvgTissueReps <- function(x) {  
  Input <- x
  R03 <- subset(R03_All_Tall, Tissue==Input)  
  R03_Rep1 <- subset(R03, Replicate=="Rep1")
  R03_Rep1 <- rename(R03_Rep1, c(Log2signal="Rep1Log2signal"))
  R03_Rep2 <- subset(R03, Replicate=="Rep2")
  R03_Rep2 <- rename(R03_Rep2, c(Log2signal="Rep2Log2signal"))
  R03_Rep3 <- subset(R03, Replicate=="Rep3")
  R03_Rep3 <- rename(R03_Rep3, c(Log2signal="Rep3Log2signal"))  
  R03_Wide <- cbind(R03_Rep1[2:9],
                    R03_Rep1[11],
                    R03_Rep1$Rep1Log2signal,
                    R03_Rep2$Rep2Log2signal,
                    R03_Rep3$Rep3Log2signal)
  R03_Mean   <- apply(R03_Wide[10:12],1,mean, na.rm=TRUE)
  R03_SD     <- apply(R03_Wide[10:12],1,sd, na.rm=TRUE)
  R03_MeanSD <- cbind(R03_Wide,R03_Mean,R03_SD)
  return(R03_MeanSD)
}
###################################

### Analyze NEAT tissues: Brain, Liver, and Placenta
### Calculate R03 Mean and SD
R03_Brain_Avg    <- AvgTissueReps("Brain")
R03_Liver_Avg    <- AvgTissueReps("Liver")
R03_Placenta_Avg <- AvgTissueReps("Placenta")

R03_BLP_Avg <- rbind(R03_Brain_Avg,
                     R03_Liver_Avg,
                     R03_Placenta_Avg)

### Calculate T1vT2 Ratios
M  <-      R03_Brain_Avg$R03_Mean - R03_Liver_Avg$R03_Mean
SD <- sqrt(R03_Brain_Avg$R03_SD^2 + R03_Liver_Avg$R03_SD^2)
A  <-     (R03_Brain_Avg$R03_Mean + R03_Liver_Avg$R03_Mean)/2
BvL_R <- cbind(R03_Brain_Avg[1:8],M,A,SD)
BvL_R$T1vT2 <- "BvL"

M  <-      R03_Placenta_Avg$R03_Mean - R03_Brain_Avg$R03_Mean
SD <- sqrt(R03_Placenta_Avg$R03_SD^2 + R03_Brain_Avg$R03_SD^2)
A  <-     (R03_Placenta_Avg$R03_Mean + R03_Brain_Avg$R03_Mean)/2
PvB_R <- cbind(R03_Brain_Avg[1:8],M,A,SD)
PvB_R$T1vT2 <- "PvB"

M  <-      R03_Placenta_Avg$R03_Mean - R03_Liver_Avg$R03_Mean
SD <- sqrt(R03_Placenta_Avg$R03_SD^2 + R03_Liver_Avg$R03_SD^2)
A  <-     (R03_Placenta_Avg$R03_Mean + R03_Liver_Avg$R03_Mean)/2
PvL_R <- cbind(R03_Brain_Avg[1:8],M,A,SD)
PvL_R$T1vT2 <- "PvL"

R03_T1vT2_ALL <- rbind(BvL_R, PvB_R, PvL_R)

### DotBar Plots for All miRs-of-Interest (MOI) by T1vT2 Ratios
Mature_Name <- c("miR-451a","miR-375","miR-335-5p","miR-218-5p","miR-125b-5p")     # Toggle MOI
Selected    <- c("MOI","MOI","MOI","MOI","MOI")                                    # Toggle MOI
# Mature_Name <- c("miR-451a","miR-9-5p")    # Toggle MOI
# Selected    <- c("MOI","MOI")              # Toggle MOI

MOI_R03 <- cbind(Mature_Name,Selected)

R03_T1vT2_ALL_MOI <- merge(R03_T1vT2_ALL, MOI_R03, by="Mature_Name", all.x=TRUE)

Tech_Key <- read.delim("Tech_Key.txt")
R03_T1vT2_ALL_MOI_Tech <- merge(R03_T1vT2_ALL_MOI, Tech_Key, by="Site_Platform", all.x=TRUE)

R03_T1vT2_ALL_MOI_Tech_Rd <- R03_T1vT2_ALL_MOI_Tech
R03_T1vT2_ALL_MOI_Tech_Rd$Round <- "R03" 

R03_MOI <- data.frame(subset(R03_T1vT2_ALL_MOI_Tech, Selected=="MOI"))

R03_MOI_OBSmedian <- ddply (R03_MOI,
                            .(T1vT2, Mature_Name),
                            summarize,
                            medianRatio = median(M, na.rm=TRUE))
R03_MOI_OBS_mad <- ddply (R03_MOI,
                          .(T1vT2, Mature_Name),
                          summarize,
                          medianMAD = mad(M, na.rm=TRUE))
R03_MOI_OBS <- merge(R03_MOI_OBSmedian, R03_MOI_OBS_mad, by=c("T1vT2","Mature_Name"))
R03_MOI_OBS$plusMAD <- (R03_MOI_OBS$medianRatio + R03_MOI_OBS$medianMAD)
R03_MOI_OBS$minusMAD <- (R03_MOI_OBS$medianRatio - R03_MOI_OBS$medianMAD)

R03_hline_MED_T1vT2.data <- rename(R03_MOI_OBSmedian, c(medianRatio="z"))

### Order MOIs by Relative Abundance
R03_T1vT2_ALL_MOI_Tech$Mature_Name_f = factor(R03_T1vT2_ALL_MOI_Tech$Mature_Name,
                                              levels=c('miR-451a',
                                                       'miR-125b-5p',
                                                       'miR-218-5p',
                                                       'miR-335-5p',
                                                       'miR-375'))
R03_MOI_OBS$Mature_Name_f = factor(R03_MOI_OBS$Mature_Name,
                                   levels=c('miR-451a',
                                            'miR-125b-5p',
                                            'miR-218-5p',
                                            'miR-335-5p',
                                            'miR-375'))

### Order Pairs by MixOrder
R03_T1vT2_ALL_MOI_Tech$T1vT2_f = factor(R03_T1vT2_ALL_MOI_Tech$T1vT2,
                                        levels=c('PvB',
                                                 'PvL',
                                                 'BvL'))
R03_MOI_OBS$T1vT2_f = factor(R03_MOI_OBS$T1vT2,
                             levels=c('PvB',
                                      'PvL',
                                      'BvL'))

### Analyze mixtures: Mix1 and Mix2
### Calculate R03 Mean and SD
R03_Mix1_Avg    <- AvgTissueReps("Mix1")
R03_Mix2_Avg    <- AvgTissueReps("Mix2")

R03_M1vM2_Avg <- rbind(R03_Mix1_Avg,
                       R03_Mix2_Avg)

### In Silico Modeling of Mixtures
LinB_R03 <- 2^R03_Brain_Avg$R03_Mean
LinL_R03 <- 2^R03_Liver_Avg$R03_Mean
LinP_R03 <- 2^R03_Placenta_Avg$R03_Mean

LinLP_R03 <- cbind(LinL_R03, LinP_R03)
LinBP_R03 <- cbind(LinB_R03, LinP_R03)
LinBL_R03 <- cbind(LinB_R03, LinL_R03)

LinLP_R03_max <- apply(LinLP_R03[,1:2],1,max,na.rm=TRUE)
LinBP_R03_max <- apply(LinBP_R03[,1:2],1,max,na.rm=TRUE)
LinBL_R03_max <- apply(LinBL_R03[,1:2],1,max,na.rm=TRUE)

TSI_B_R03 <- LinB_R03/LinLP_R03_max
TSI_L_R03 <- LinL_R03/LinBP_R03_max
TSI_P_R03 <- LinP_R03/LinBL_R03_max

# Identify 10X Tissue-Selective miRNAs
TSI_Class <- ifelse(TSI_B_R03 >=10 , "10X_Brain",
                    ifelse(TSI_L_R03 >=10, "1_to_1", # 10X_Liver incorporated into 1_to_1 TSI_Class
                           ifelse(TSI_P_R03 >=10, "10X_Placenta",
                                  ifelse(abs(log2(LinB_R03/LinP_R03/LinL_R03)) ==0.0, "NS",
                                         ifelse(abs(log2(LinB_R03/LinP_R03)) <=0.075, "1_to_1", "NS")))))

TSI_R03 <- cbind(TSI_B_R03, TSI_L_R03, TSI_P_R03, TSI_Class)

ModM1_R03 <- log2(((LinB_R03*0.25)/Bf)+((LinL_R03*0.25)/Lf)+((LinP_R03*0.5)/Pf))
ModM2_R03 <- log2(((LinB_R03*0.5)/Bf)+((LinL_R03*0.25)/Lf)+((LinP_R03*0.25)/Pf))

P <- ModM1_R03 - ModM2_R03

### Calculate M1vM2 Ratios
M  <-      R03_Mix1_Avg$R03_Mean - R03_Mix2_Avg$R03_Mean
SD <- sqrt(R03_Mix1_Avg$R03_SD^2 + R03_Mix2_Avg$R03_SD^2)
A  <-     (R03_Mix1_Avg$R03_Mean + R03_Mix2_Avg$R03_Mean)/2
DFP <- M-P

R03_M1vM2_R <- cbind(R03_Mix1_Avg[1:8],M,A,SD,P,DFP)
R03_M1vM2_R$T1vT2 <- "M1vM2"

### DotBar Plots for All miRs-of-Interest (MOI) by M1vM2 Ratios
Mature_Name <- c("miR-451a","miR-375","miR-335-5p","miR-218-5p","miR-125b-5p")
Selected    <- c("MOI","MOI","MOI","MOI","MOI")
# Mature_Name <- c("miR-451a","miR-9-5p")
# Selected    <- c("MOI","MOI")

MOI_R03 <- cbind(Mature_Name,Selected)

R03_M1vM2_R_MOI <- merge(R03_M1vM2_R, MOI_R03, by="Mature_Name", all.x=TRUE)

Tech_Key <- read.delim("Tech_Key.txt")
R03_M1vM2_R_MOI_Tech <- merge(R03_M1vM2_R_MOI, Tech_Key, by="Site_Platform", all.x=TRUE)

R03_MOI <- data.frame(subset(R03_M1vM2_R_MOI_Tech, Selected=="MOI"))

R03_MOI_OBSmedian <- ddply (R03_MOI,
                            .(T1vT2, Mature_Name),
                            summarize,
                            medianRatio = median(M, na.rm=TRUE))
R03_MOI_OBS_mad <- ddply (R03_MOI,
                          .(T1vT2, Mature_Name),
                          summarize,
                          medianMAD = mad(M, na.rm=TRUE))
R03_MOI_OBS <- merge(R03_MOI_OBSmedian, R03_MOI_OBS_mad, by=c("T1vT2","Mature_Name"))
R03_MOI_OBS$plusMAD <- (R03_MOI_OBS$medianRatio + R03_MOI_OBS$medianMAD)
R03_MOI_OBS$minusMAD <- (R03_MOI_OBS$medianRatio - R03_MOI_OBS$medianMAD)

R03_hline_MED_M1vM2.data <- rename(R03_MOI_OBSmedian, c(medianRatio="z"))

### Order MOIs by Relative Abundance
R03_M1vM2_R_MOI_Tech$Mature_Name_f = factor(R03_M1vM2_R_MOI_Tech$Mature_Name,
                                            levels=c('miR-451a',
                                                     'miR-125b-5p',
                                                     'miR-335-5p',
                                                     'miR-218-5p',
                                                     'miR-375'))
R03_MOI_OBS$Mature_Name_f = factor(R03_MOI_OBS$Mature_Name,
                                   levels=c('miR-451a',
                                            'miR-125b-5p',
                                            'miR-335-5p',
                                            'miR-218-5p',
                                            'miR-375'))

R03_M1vM2_R[R03_M1vM2_R=="-Inf"]<-NaN
R03_M1vM2_R[R03_M1vM2_R=="Inf"]<-NaN

### Calculate Average DFP per Site_Platform
R03_DFP_avg <- ddply (R03_M1vM2_R,
                      .(Site_Platform),
                      summarize,
                      DFPavg = mean(DFP, na.rm=TRUE))
R03_DFP_sd <- ddply (R03_M1vM2_R,
                     .(Site_Platform),
                     summarize,
                     DFPsd = sd(DFP, na.rm=TRUE))
R03_DFP_mad <- ddply (R03_M1vM2_R,
                      .(Site_Platform),
                      summarize,
                      DFPmad = mad(DFP, na.rm=TRUE))
R03_DFP <- cbind(R03_DFP_avg,R03_DFP_sd$DFPsd,R03_DFP_mad$DFPmad)
R03_DFP_Tech <- merge(R03_DFP, Tech_Key, by="Site_Platform", all.x=TRUE)
R03_DFP_Tech$Round <- "R01"
R03_DFP_Tech <- rename(R03_DFP_Tech, c("R03_DFP_sd$DFPsd" = "DFPsd"))
R03_DFP_Tech <- rename(R03_DFP_Tech, c("R03_DFP_mad$DFPmad" = "DFPmad"))

R03_hline_DFP.data <- rename(R03_DFP_avg, c(DFPavg="z"))

### Annotate GenomeScale with Lab001_A PCR 
# R03_M1vM2_PCR <- subset(R03_M1vM2_R_MOI_Tech, Site_Platform=="Lab001_A")
# Mature_Name <- R03_M1vM2_PCR[2]
# PCR_DFP <- R03_M1vM2_PCR[13]
# R03_M1vM2_PCR_DFP <- cbind(Mature_Name,PCR_DFP)
# R03_M1vM2_GenScale_PCR <- merge(R03_M1vM2_R_MOI_Tech, R03_M1vM2_PCR_DFP, by=c("Mature_Name"))
# R03_M1vM2_GenScale_PCR_MOI <- subset(R03_M1vM2_GenScale_PCR, Selected=="MOI")

##################
#####  Done  #####

A_ModM1M2_R03 <- (ModM1_R03 + ModM2_R03)/2
M_ModM1M2_R03 <- ModM1_R03 - ModM2_R03

Mix1_v_Mix2 <- cbind(R03_M1vM2_R, A_ModM1M2_R03, M_ModM1M2_R03, TSI_Class)


# Select Site_Platform for Bland-Altman Plots 
Mix1_v_Mix2_SitePlat     <- subset(Mix1_v_Mix2, Site_Platform==SP_R03) # Replace "Lab______"
Mix1_v_Mix2_SitePlat_A   <- subset(Mix1_v_Mix2_SitePlat, TSI_Class=="1_to_1")
Mix1_v_Mix2_SitePlat_A$labels <- "0"
Mix1_v_Mix2_SitePlat_B   <- subset(Mix1_v_Mix2_SitePlat, TSI_Class=="10X_Brain")
Mix1_v_Mix2_SitePlat_B$labels <- "1"
Mix1_v_Mix2_SitePlat_C   <- subset(Mix1_v_Mix2_SitePlat, TSI_Class=="10X_Placenta")
Mix1_v_Mix2_SitePlat_C$labels <- "1"

Mix1_v_Mix2_SitePlat_ABC <- rbind(Mix1_v_Mix2_SitePlat_A, Mix1_v_Mix2_SitePlat_B, Mix1_v_Mix2_SitePlat_C)

# BAplot of PREDICTED Mixtures (w/ non-selective)
ggplot(Mix1_v_Mix2_SitePlat, aes(A_ModM1M2_R03, M_ModM1M2_R03,
                                 fill=TSI_Class,
                                 shape=TSI_Class,
                                 alpha=TSI_Class
                                 ))  +
  geom_point(size=3) +
  geom_smooth(data=Mix1_v_Mix2_SitePlat_ABC , method="loess", span=1.0, size=0.5, alpha=0.3, color="black") +
  scale_fill_manual(values = c("yellow","blue", "red", "black")) +
  scale_shape_manual(values = c(21,21,21,1)) +
  scale_alpha_manual(values = c(1.0,1.0,1.0,0.5)) +
  geom_hline(aes(yintercept = 1), color="red", size=1.0, linetype=2) +
  geom_hline(aes(yintercept = 0), color="orange", size=1.0, linetype=2) +
  geom_hline(aes(yintercept = -1), color="blue", size=1.0, linetype=2) +
  ylim(-2.5,2.5) +
  xlim(0,20) +
  theme(legend.position = "none") +
  labs(x = "Average Log2Signal",
       y = "Predicted Log2ratio",
       title = paste("Site = ", SP_R03))

### BAplot of OBSERVED Mixtures (w/ non-selective)
ggplot(Mix1_v_Mix2_SitePlat, aes(A, M,
                                 fill=TSI_Class,
                                 shape=TSI_Class,
                                 alpha=TSI_Class
                                 ))  +
  geom_point(size=3) +
  geom_smooth(data=Mix1_v_Mix2_SitePlat_ABC , method="loess", span=1.0, size=0.5, alpha=0.3, color="black") +
  scale_fill_manual(values = c("yellow","blue", "red", "black")) +
  scale_shape_manual(values = c(21,21,21,1)) +
  scale_alpha_manual(values = c(1.0,1.0,1.0,0.5)) +
  geom_hline(aes(yintercept = 1), color="red", size=1.0, linetype=2) +
  geom_hline(aes(yintercept = 0), color="orange", size=1.0, linetype=2) +
  geom_hline(aes(yintercept = -1), color="blue", size=1.0, linetype=2) +
  ylim(-2.5,2.5) +
  xlim(0,20) +
  theme(legend.position = "none") +
  labs(x = "Average Log2Signal",
       y = "Observed Log2ratio",
       title = paste("Site = ", SP_R03))

### Tolerance calculations
bw<-2
tol<-(IQR(Mix1_v_Mix2_SitePlat$DFP, na.rm=TRUE))
bias<-median(Mix1_v_Mix2_SitePlat$DFP, na.rm=TRUE)
prop<-0.8

### Bias Corrected LLOP
A<-Mix1_v_Mix2_SitePlat$A
DFP<-Mix1_v_Mix2_SitePlat$DFP-bias
drop<-which(is.na(A)|is.na(DFP))
A<-A[-drop]
DFP<-DFP[-drop]
t.A<-seq(min(A),max(A),length.out=100)
A<-A[order(DFP)]
DFP<-sort(DFP)
dist<-abs(scale(matrix(A,length(A),length(t.A)),center=t.A,scale=FALSE))
w<-exp(-dist^2/bw^2)
w<-scale(w,center=FALSE,scale=colSums(w))
ind<-which(abs(DFP)<abs(tol))
LLOP <-t.A[min(which(colSums(w[ind,])>prop))]
LLOP

### BAplot of DFP
ggplot(Mix1_v_Mix2_SitePlat, aes((A+A_ModM1M2_R03)/2, M-M_ModM1M2_R03,
                                 fill=TSI_Class,
                                 shape=TSI_Class,
                                 alpha=TSI_Class
                                 ))  +
  geom_hline(aes(yintercept = 0), color="white", size=1, linetype=1) +
  geom_point(size=3) +
  scale_fill_manual(values = c("yellow","blue", "red", "black")) +
  scale_shape_manual(values = c(21,21,21,1)) +
  scale_alpha_manual(values = c(1.0,1.0,1.0,0.5)) +
  geom_hline(aes(yintercept = bias), color="black", size=.75, linetype=1) +
  geom_hline(aes(yintercept = tol+bias), color="black", size=.75, linetype=2) +
  geom_hline(aes(yintercept = -tol+bias), color="black", size=.75, linetype=2) +
  geom_vline(aes(xintercept = LLOP), color="red", size=.75, linetype=1) +
  ylim(-3,3) +
  scale_x_continuous(limits=c(0,20),breaks=c(0,5,10,15,20,signif(LLOP,3))) +
  theme(legend.position = "none") +
  labs(x = "Average Log2Signal",
       y = "Observed - Predicted",
       title = paste("Site = ", SP_R03, "   Median DFP Log2Ratios = ", signif(bias,3), "   IQR = ", signif(tol,3), "   LLOP = ", signif(LLOP,3)))

### BoxPlots by TSI Class
ggplot(na.omit(Mix1_v_Mix2_SitePlat), aes(TSI_Class, M-M_ModM1M2_R03, fill=TSI_Class))  +
  geom_hline(aes(yintercept = 0), color="white", size=1, linetype=1) +
  scale_fill_manual(values = c("yellow","blue", "red", "darkgrey")) +
  geom_boxplot(outlier.shape=95, outlier.size=5, outlier.colour = "black") +
  geom_hline(aes(yintercept = bias), color="black", size=.75, linetype=1) +
  geom_hline(aes(yintercept = tol+bias), color="black", size=.75, linetype=2) +
  geom_hline(aes(yintercept = -tol+bias), color="black", size=.75, linetype=2) +
  scale_x_discrete(limits=c("1_to_1","10X_Brain","10X_Placenta","NS")) +
  ylim(-3,3) +
  theme(legend.position = "none") +
  labs(
       y = "Observed - Predicted",
       title = paste("Site = ", SP_R03))

### BarChart of TSI Classes
ggplot(na.omit(Mix1_v_Mix2_SitePlat), aes(TSI_Class, fill=TSI_Class)) +
  scale_fill_manual(values = c("yellow","blue", "red", "darkgrey")) +
  geom_bar(colour="black") +
  theme(legend.position = "none") +
  labs(title = paste("Site = ", SP_R03,
                     "          Detected miRNA = ",length(na.omit(Mix1_v_Mix2_SitePlat$TSI_Class))))

### ROCplot

R03_M1M2_Wide <-cbind(R03_Mix1_Avg[1:12], R03_Mix2_Avg[10:12])

R03_M1M2_Wide_Lab <- subset(R03_M1M2_Wide, Site_Platform==SP_R03)

### Require minimum of 2 non-NA Mix1
R03_M1M2_Wide_Lab <- R03_M1M2_Wide_Lab[rowSums(is.na(R03_M1M2_Wide_Lab[10:12]))
                                       <(length(R03_M1M2_Wide_Lab[10:12])-1),]

### Require minimum of 2 non-NA Mix2
R03_M1M2_Wide_Lab <- R03_M1M2_Wide_Lab[rowSums(is.na(R03_M1M2_Wide_Lab[13:15]))
                                       <(length(R03_M1M2_Wide_Lab[13:15])-1),]

### T-Test
R03_M1M2_Wide_Lab$Pval <- sapply(1:nrow(R03_M1M2_Wide_Lab),
                                 function(i) t.test(R03_M1M2_Wide_Lab[i,10:12],
                                                    R03_M1M2_Wide_Lab[i,13:15])$p.value)

R03_M1M2_Wide_Lab_annot <- merge(R03_M1M2_Wide_Lab, Mix1_v_Mix2_SitePlat_ABC, by="Mature_Name", all.y=TRUE)
R03_M1M2_Wide_Lab_annot$predictions <- 1 - R03_M1M2_Wide_Lab_annot$Pval

library(ROCR)

pred_ALL <- prediction(R03_M1M2_Wide_Lab_annot$predictions,
                       R03_M1M2_Wide_Lab_annot$labels)

perf_ROC_ALL <- performance(pred_ALL,"tpr","fpr")

ALL_x <- data.frame(perf_ROC_ALL@x.values)
FPR <-ALL_x[,1]

ALL_y <- data.frame(perf_ROC_ALL@y.values)
TPR <-ALL_y[,1]

ALL_xy <- data.frame(cbind(TPR,FPR))

perf_AUC_ALL <- performance(pred_ALL,"auc")
AUC <- unlist(slot(perf_AUC_ALL, "y.values"))

ggplot(ALL_xy, aes(x=FPR, y=TPR)) +
  geom_line(size=2) +
  labs(title = paste("Site = ", SP_R03,"          AUC = ", signif(AUC,3)))

############
### DONE ###
############

