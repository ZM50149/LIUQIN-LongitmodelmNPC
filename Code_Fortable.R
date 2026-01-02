library(Hmisc)
library(readxl)
library(writexl)
library(openxlsx)
library(survival)
library(survminer)
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(missForest)
library(caret)
library(pcutils)
library(lcmm)
library(openxlsx)
library(ggplot2)
library(palmerpenguins)
library(gghalves)
library(ggdist)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(ggpmisc)
library(psych)
library(fdapace)
library(rainbow)
library(tableone)
library(survey)
library(glmnet)
library(randomForestSRC)
library(pec)


rm(list =ls())
main_path <- 'C:/Users'

df_baseline <- read_excel(file.path(main_path,'Data_prep/Data_analysis.xlsx'), sheet = "Sheet1")
df_follow <- read_excel(file.path(main_path,'Data_prep/Data_analysis.xlsx'), sheet = "Sheet2")

df_baseline <- df_baseline[df_baseline$Group == 0,]
df_follow <- df_follow[df_follow$Group == 0,]

Index_train <- which(df_baseline$TrainTest == 0)
Index_test <- which(df_baseline$TrainTest == 1)


var_clinic <- c("Age","BMI","KPS", "Gender","Marriage",
                "Smoking","Drinking","Hypertension","Diabetes","Coronary","FamilyHistory",
                "Histology","MeteBone","MeteLiver","MeteLung","MeteOthers","cT","cN","Biotherapy","chemotherapy_regimens")
var_baseline <- c("EBV","CRP","WBC","LY","MO","NE","HGB","PLT","ALB","ALBGLO","LDH","ALP",
                  "CAR", "CALLY", "PAR", "NAR", "LCR", "PLR", "NLR", "dNLR", "SII", "SIRI",
                  "IBI","GPS", "mGPS", "LCS", "NC", "PC", "NP", "LA", "MLR", "NMLR")


var_follclass <- c("EBVclass", "CRPclass", "WBCclass", "LYclass", "MOclass", "NEclass", "HGBclass",
                   "PLTclass", "ALBclass", "ALBGLOclass", "LDHclass", "ALPclass", "CARclass", "CALLYclass",
                   "PARclass", "NARclass", "LCRclass", "PLRclass", "NLRclass","dNLRclass", "SIIclass",
                   "SIRIclass", "IBIclass", "GPSclass", "mGPSclass", "LCSclass", "NCclass", "PCclass", "NPclass",
                   "LAclass", "MLRclass", "NMLRclass")

var_follpfcs <- c("EBVX1", "EBVX2", "EBVX3", "CRPX1", "CRPX2", "CRPX3", "CRPX4",
                  "WBCX1", "WBCX2", "WBCX3", "WBCX4", "WBCX5", "WBCX6", 
                  "LYX1", "LYX2", "LYX3", "LYX4",
                  "MOX1", "MOX2", "MOX3", "MOX4", "MOX5", "MOX6", 
                  "NEX1", "NEX2", "NEX3", "NEX4", "NEX5", "NEX6",
                  "HGBX1", "HGBX2", "HGBX3", "PLTX1", "PLTX2", "PLTX3", "PLTX4", "PLTX5",
                  "ALBX1", "ALBX2", "ALBX3", "ALBX4", "ALBGLOX1", "ALBGLOX2", "ALBGLOX3", 
                  "LDHX1", "LDHX2", "LDHX3",
                  "ALPX1", "ALPX2", "ALPX3", "CARX1", "CARX2", "CARX3", "CARX4", 
                  "CALLYX1", "CALLYX2", "CALLYX3", "CALLYX4",
                  "PARX1", "PARX2", "PARX3", "PARX4", "PARX5", 
                  "NARX1", "NARX2", "NARX3", "NARX4", "NARX5", "LCRX1", "LCRX2", "LCRX3", "LCRX4",
                  "PLRX1", "PLRX2", "PLRX3", "PLRX4", "NLRX1", "NLRX2", "NLRX3", "NLRX4", "NLRX5",
                  "dNLRX1", "dNLRX2", "dNLRX3",  "dNLRX4",  "dNLRX5", "SIIX1", "SIIX2", "SIIX3", "SIIX4", "SIIX5",
                  "SIRIX1", "SIRIX2", "SIRIX3", "SIRIX4", "SIRIX5", "IBIX1", "IBIX2", "IBIX3", "IBIX4",
                  "GPSX1", "GPSX2", "GPSX3", "GPSX4", "GPSX5", "mGPSX1", "mGPSX2", "mGPSX3", "mGPSX4", "mGPSX5",
                  "LCSX1", "LCSX2", "LCSX3", "LCSX4", "LCSX5", "NCX1", "NCX2", "NCX3", "NCX4", "NCX5",
                  "PCX1", "PCX2", "PCX3", "PCX4", "NPX1", "NPX2", "NPX3", "NPX4", "NPX5", 
                  "LAX1", "LAX2", "LAX3", "LAX4",
                  "MLRX1", "MLRX2", "MLRX3", "MLRX4", "MLRX5", 
                  "NMLRX1", "NMLRX2", "NMLRX3", "NMLRX4", "NMLRX5")

var_fp <- c("clinic",	"baseline",	"lctm",	"fpca",	"clinbase", "clinebv",	"clinbaselctm",	"clinbasepfcs", 	"clinbaselctmpfc")


num <- c("Num","Age","BMI","KPS", "OS","DFS","FollowUP", var_baseline, var_follpfcs)
df_baseline[num] <- lapply(df_baseline[num], as.numeric)

fac <- c("Gender","Marriage",
         "Smoking","Drinking","Hypertension","Diabetes","Coronary","FamilyHistory",
         "Histology","MeteBone","MeteLiver","MeteLung","MeteOthers","cT","cN","cM",
         "Radiotherapy","Biotherapy", "GPS", "mGPS", "LCS", var_follclass)
df_baseline[fac] <- lapply(df_baseline[fac], as.factor)

num <- c("Num","Chemotherapy", var_baseline)
df_follow[num] <- lapply(df_follow[num], as.numeric)

fac <- c("GPS", "mGPS", "LCS")
df_follow[fac] <- lapply(df_follow[fac], as.factor)


log_columns <- c("EBV","CRP","WBC","LY","MO","NE","HGB","PLT","ALBGLO","LDH","ALP",
                 "CAR", "CALLY", "PAR", "NAR", "LCR", "PLR", "NLR",  "SII", "SIRI",
                 "IBI","NC", "PC", "NP", "LA", "MLR", "NMLR","ALB","NLR","dNLR")
df_follow[log_columns] <- log(df_follow[log_columns] + 1)
df_baseline[log_columns] <- log(df_baseline[log_columns] + 1)



mydata <- df_baseline
mydata$KPScat <- ifelse(mydata$KPS < 90,'< 90','>=90')
sum_vars <- c(var_clinic, "DFS", "FollowUP","KPScat")
fac_vars <- c("Gender","Marriage", "Smoking","Drinking","Hypertension","Diabetes","Coronary","FamilyHistory",
              "Histology","MeteBone","MeteLiver","MeteLung","MeteOthers","cT","cN","Biotherapy","chemotherapy_regimens","KPScat")
Tab1 <- Table_f(mydata, sum_vars, 'OS')






