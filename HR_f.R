hr_f <- function(mydata, facp){
  results <- data.frame(
    Variable = character(),
    HR = numeric(),
    HR_lower_95 = numeric(),
    HR_upper_95 = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE)
  
  for (var in facp) {
    formula <- as.formula(paste("Surv(FollowUP, DFS)~", var))
    fit <- coxph(formula, data = mydata)  
    sum_fit <- summary(fit)
    coef <- sum_fit$coefficients
    results <- rbind(results, data.frame(
      Variable = var,
      HR = coef[, "exp(coef)"],
      HR_lower_95 = sum_fit$conf.int[, "lower .95"],
      HR_upper_95 = sum_fit$conf.int[, "upper .95"],
      P_value = round(coef[, "Pr(>|z|)"],digits = 3)
    ))
  }
  
  
  format_p <- function(p) {
    ifelse(p < 0.001,
           "P<0.001",
           paste0("P=", formatC(round(p, 3), format = "f", digits = 3)))
  }
  
  results$HRs <- paste0(round(results$HR,2), ' (', 
                        round(results$HR_lower_95,2),'-', 
                        round(results$HR_upper_95,2),')', ', ',
                        format_p(results$P_value))
  results
}