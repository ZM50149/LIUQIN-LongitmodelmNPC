##============================================================
## Kaplan-Meier Survival Analysis Across Multiple Prognostic Models
##
## Purpose:
##   This code performs risk stratification and Kaplan-Meier survival
##   analysis for multiple prognostic scores. For each prognostic model,
##   the optimal cut-off value is determined in the training cohort using
##   surv_cutpoint(). Patients in the full cohort are then classified into
##   low- and high-risk groups according to this cut-off. Kaplan-Meier
##   curves are generated separately in the training and validation cohorts.
##
## Main steps:
##   1. Define prognostic score variables from different models.
##   2. For each prognostic score, determine the optimal cut-off in the
##      training cohort and generate Kaplan-Meier curves.
##============================================================


##============================================================
## 1. Define prognostic variables
##============================================================

var_fp <- c(
  "clinic",
  "baseline",
  "lctm",
  "fpca",
  "clinbase",
  "clinebv",
  "clinbaselctm",
  "clinbasepfcs",
  "clinbaselctmpfc"
)


mydata <- df_baseline

km_results <- list()


##============================================================
## 2. Loop over each prognostic model score
##============================================================

for (fp in var_fp) {
    pred_cut <- surv_cutpoint(
    mydata[Index_train, ],
    time      = "FollowUP",
    event     = "OS",
    variables = fp
  )
  pred_cut_value <- pred_cut[["cutpoint"]][["cutpoint"]]
  risk_var <- paste0("risk_group_", fp)
  mydata[[risk_var]] <- ifelse(
    mydata[[fp]] > pred_cut_value,
    "BHigh",
    "ALow"
  )
  mydata[[risk_var]] <- factor(
    mydata[[risk_var]],
    levels = c("ALow", "BHigh")
  )
  p_train <- ggsurvplot(
    survfit(
      as.formula(
        paste0("Surv(FollowUP, DFS) ~ ", risk_var)
      ),
      data = mydata[Index_train, ]
    ),
    conf.int    = TRUE,
    pval        = TRUE,
    pval.method = TRUE,
    xlim        = c(0, 84),
    break.x.by  = 12,
    risk.table  = TRUE,
    censor      = TRUE,
    censor.size = 3,
    axes.offset = FALSE,
    palette     = c("#3E608D", "#4E1945"),
    title       = paste0(fp, " - Training cohort"),
    legend.title = "Risk group",
    legend.labs  = c("Low risk", "High risk"),
    xlab        = "Follow-up time, months",
    ylab        = "Disease-free survival probability"
  )
  p_test <- ggsurvplot(
    survfit(
      as.formula(
        paste0("Surv(FollowUP, DFS) ~ ", risk_var)
      ),
      data = mydata[Index_test, ]
    ),
    conf.int    = TRUE,
    pval        = TRUE,
    pval.method = TRUE,
    xlim        = c(0, 84),
    break.x.by  = 12,
    risk.table  = TRUE,
    censor      = TRUE,
    censor.size = 3,
    axes.offset = FALSE,
    palette     = c("#3E608D", "#4E1945"),
    title       = paste0(fp, " - Validation cohort"),
    legend.title = "Risk group",
    legend.labs  = c("Low risk", "High risk"),
    xlab        = "Follow-up time, months",
    ylab        = "Disease-free survival probability"
  )
  
  km_results[[fp]] <- list(
    cutpoint   = pred_cut_value,
    risk_var   = risk_var,
    train_plot = p_train,
    test_plot  = p_test
  )
  print(p_train)
  print(p_test)
}
