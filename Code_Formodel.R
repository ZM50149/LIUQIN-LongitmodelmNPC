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


var_clin_ebv <- c(var_clinic_aic, "EBV")
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_ebv,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)
summary(fit)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinebv <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

var_clin_base <- c(var_clinic_aic, var_baseline_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbase <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex

var_clin_base_lctm <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_lctm,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbaselctm <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


var_clin_base_pfcs <- c(var_clinic_aic, var_baseline_aic, var_pfcs_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_pfcs,collapse = '+')))
fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbasepfcs <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


mydata <- df_baseline[Index_train,]
var_clin_base_lctmpfcs <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic, var_pfcs_aic)
tmp_formu <- as.formula(paste0('Surv(FollowUP, OS)~', paste0(var_clin_base_lctmpfcs,collapse = '+')))

fit <- coxph(tmp_formu, data = mydata)

tmp <- cindex_f(df_baseline, Index_train, fit)
fp_df$clinbaselctmpfc <- tmp$fp
cindex_df <- rbind(cindex_df, tmp$cindex)
tmp$cindex


var_fp <- c("clinic",	"baseline",	"lctm",	"fpca",	"clinbase", "clinebv",	"clinbaselctm",	"clinbasepfcs", 	"clinbaselctmpfc")

mydata <- df_baseline
pred_cut <- surv_cutpoint(mydata[Index_train, ], time = "FollowUP", event = "OS", variables ="clinbaselctmpfc")
pred_cut_value <- pred_cut[["cutpoint"]][["cutpoint"]]
df_baseline$risk_group_clinbaselctmpfc <- ifelse(df_baseline$clinbaselctmpfc > pred_cut_value, "BHigh", "ALow")


var_list <- list(
  var_clinic_aic,
  var_baseline_aic,
  var_lctm_aic,
  var_pfcs_aic,
  var_clin_ebv,
  var_clin_base,
  var_clin_base_lctm,
  var_clin_base_pfcs,
  var_clin_base_lctmpfcs
)

for (i in seq_along(var_list)) {
  var_vec <- var_list[[i]]
  formu <- as.formula(
    paste0("Surv(FollowUP, OS) ~ ", paste(var_vec, collapse = " + "))
  )
  
  fit_obj <- coxph(formu, data = mydata)
  
  # 生成 fit1, fit2, fit3 ...
  assign(paste0("fit", i), fit_obj)
}


fit_names <- paste0("fit", 1:9)         
fit_list  <- lapply(fit_names, get)   
names(fit_list) <- fit_names 

plot1 <- dca(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,times = 12)
ggplot(plot1)

plot2 <- dca(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,times = 36)
ggplot(plot2)

plot3 <- dca(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,times = 60)
ggplot(plot3)



