cindex_f <- function(mydata, Index_train, fit){
  tmp_surv <- Surv(mydata[Index_train,]$FollowUP, mydata[Index_train,]$OS)
  fp <- predict(fit, mydata)
  C_train <- 1-rcorr.cens(fp[Index_train],Surv(mydata[Index_train,]$FollowUP, mydata[Index_train,]$OS))
  C_test <- 1-rcorr.cens(fp[-Index_train],Surv(mydata[-Index_train,]$FollowUP, mydata[-Index_train,]$OS))
  cindex_tmp <- rbind(C_train, C_test)
  return(list(cindex = cindex_tmp, fp = fp))
}