##============================================================
## Prognostic Model Development and Validation
##
## Purpose:
##   This script performs data preprocessing and prognostic model
##   development for patients with metastatic nasopharyngeal carcinoma.
##   Baseline clinical variables, laboratory biomarkers, longitudinal
##   trajectory classes, and FPCA-derived features are used to construct
##   multiple Cox proportional hazards models.
##
## Main steps:
##   1. Define cohort indices.
##   2. Define variables.
##   3. Convert variables to format.
##   4. Initialize objects for storing prognostic scores and C-index.
##   5. Build clinical model.
##   6. Build baseline biomarker model.
##   7. Build longitudinal trajectory class model.
##   8. Build FPCA-derived feature model using LASSO and AIC selection.
##   9. Build combined clinical plus EBV-DNA model.
##   10. Build combined clinical plus baseline biomarker model.
##   11. Build combined clinical, baseline biomarker, and trajectory model.
##   12. Build combined clinical, baseline biomarker, and FPCA model.
##   13. Build final integrated model.
##
## Notes:
##   - df_baseline contains baseline patient-level data.
##   - df_follow contains longitudinal repeated biomarker data.
##   - OS is the event indicator, where 1 indicates death and 0 indicates censored/surviving status.
##   - FollowUP is the follow-up time.
##   - Table_f(), hr_f(), and cindex_f() were defined before
##============================================================


##============================================================
## 1. Define cohort indices
##============================================================
rm(list =ls())
main_path <- 'C:/Users'

df_baseline <- read_excel(file.path(main_path,'Data_prep/Data.xlsx'), sheet = "Sheet1")
df_follow <- read_excel(file.path(main_path,'Data_prep/Data.xlsx'), sheet = "Sheet2")


set.seed(25)
Index_train <- createDataPartition(df_baseline$OS, p = .7,
                                   list = FALSE,
                                   times = 1)
df_baseline$TrainTest <- 0
df_baseline$TrainTest[Index_train] <- 1

Index_test <- which(df_baseline$TrainTest == 1)


##============================================================
## 2. Define variables
##============================================================
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


##============================================================
## 3. Convert variables to format
##============================================================
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


##============================================================
## 4. Initialize objects for storing prognostic scores and C-index
##============================================================
fp_df <- data.frame(matrix(nrow = nrow(df_baseline), ncol = 0))
cindex_df <- data.frame()

##============================================================
## 5. Build clinical model
##============================================================
mydata <- df_baseline[Index_train, ]
var <- var_clinic
results <- hr_f(mydata, var)
var_clinic_sig <- results$Variable[which(results$P_value < 0.05)]

var <- var_clinic_sig
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)
fit_aic <- stepAIC(fit, direction = "both")
summary(fit_aic)
var_clinic_aic <- attr(fit_aic$terms, "term.labels")

tmp <- cindex_f(df_baseline, Index_train, fit_aic)
fp_df$clinic <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

##============================================================
## 6. Build baseline biomarker model
##============================================================
mydata <- df_baseline[Index_train, ]
var <- var_baseline
results <- hr_f(mydata, var)
var_baseline_sig <- results$Variable[which(results$P_value < 0.05)]

var <- var_baseline_sig
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)
fit_aic <- stepAIC(fit, direction = "both")
var_baseline_aic <- attr(fit_aic$terms, "term.labels")

tmp <- cindex_f(df_baseline, Index_train, fit_aic)
fp_df$baseline <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


##============================================================
## 7. Build longitudinal trajectory class model
##============================================================
mydata <- df_baseline[Index_train, ]
var <- var_follclass

results <- hr_f(mydata, var)
var_lctm_sig <- results$Variable[which(results$P_value < 0.05)]

var <- var_lctm_sig
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)
fit_aic <- stepAIC(fit, direction = "both")
summary(fit_aic)
var_lctm_aic <- attr(fit_aic$terms, "term.labels")

tmp <- cindex_f(df_baseline, Index_train, fit_aic)
fp_df$lctm <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


##============================================================
## 8. Build FPCA-derived feature model using LASSO and AIC selection
##============================================================
mydata <- df_baseline[Index_train, ]
var <- var_follpfcs

tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var,collapse = '+')))
x <- model.matrix(tmp_formu, data = mydata)[, -1]
y <- Surv(mydata$FollowUP, mydata$OS)

set.seed(125)
fit_cv <- cv.glmnet(x, y, family = "cox", alpha = 1, nfolds = 5, type.measure = "C")
fit <- glmnet(x,y, family = "cox", type.measure = "C")
coef_lasso <- coef(fit, s = fit_cv$lambda.min)

tmp_var <- var[which(coef_lasso != 0)]

surv_f <- as.formula(paste0('Surv(FollowUP, OS)~',paste0(tmp_var,collapse = '+')))
cox.obj <- coxph(surv_f , data = df_baseline[Index_train,],   x = TRUE)
fit_aic <- stepAIC(cox.obj, direction = "both")
sum_hr <- summary(fit_aic)
sum_hr
data_hr <- data.frame(sum_hr[["coefficients"]])
var_pfcs_aic <- rownames(data_hr)

tmp <- cindex_f(df_baseline, Index_train, fit_aic)
fp_df$fpca <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


##============================================================
## 9. Build combined clinical plus EBV-DNA model
##============================================================
var_clin_ebv <- c(var_clinic_aic, "EBV")
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_ebv,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)
summary(fit)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinebv <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

##============================================================
## 10. Build combined clinical plus baseline biomarker model
##============================================================
var_clin_base <- c(var_clinic_aic, var_baseline_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbase <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

##============================================================
## 11. Build combined clinical, baseline biomarker, and trajectory model
##============================================================
var_clin_base_lctm <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_lctm,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbaselctm <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


##============================================================
## 12. Build combined clinical, baseline biomarker, and FPCA model
##============================================================
var_clin_base_pfcs <- c(var_clinic_aic, var_baseline_aic, var_pfcs_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_pfcs,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbasepfcs <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

##============================================================
## 13. Build final integrated model
##============================================================
mydata <- df_baseline[Index_train,]
var_clin_base_lctmpfcs <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic, var_pfcs_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_lctmpfcs,collapse = '+')))

fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbaselctmpfc <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex



