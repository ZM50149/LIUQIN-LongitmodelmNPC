##============================================================
## Function: cindex_f
## Purpose:
##   Calculate the concordance index (C-index) for a fitted Cox
##   proportional hazards model in both the training and validation
##   sets. The function first generates model-based prognostic scores
##   for all patients, then evaluates discrimination performance using
##   Harrell's C-index separately in the training and validation cohorts.
##
## Arguments:
##   mydata      : Data frame containing FollowUP, OS, and model covariates.
##   Index_train : Row indices corresponding to the training set.
##   fit         : A fitted Cox model object used to generate prognostic scores.
##
## Returns:
##   A list containing:
##     cindex : C-index results for the training and validation sets.
##     fp     : Predicted prognostic scores for all patients.
##
## Notes:
##   FollowUP is the survival time variable.
##   OS is the event indicator, where 1 indicates death and 0 indicates censoring.
##============================================================


cindex_f <- function(mydata, Index_train, fit){
  tmp_surv <- Surv(mydata[Index_train,]$FollowUP, mydata[Index_train,]$OS)
  fp <- predict(fit, mydata)
  C_train <- 1-rcorr.cens(fp[Index_train],Surv(mydata[Index_train,]$FollowUP, mydata[Index_train,]$OS))
  C_test <- 1-rcorr.cens(fp[-Index_train],Surv(mydata[-Index_train,]$FollowUP, mydata[-Index_train,]$OS))
  cindex_tmp <- rbind(C_train, C_test)
  return(list(cindex = cindex_tmp, fp = fp))
}