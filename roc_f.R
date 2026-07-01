##============================================================
## Function: roc_f
##
## Purpose:
##   This function evaluates the time-dependent discrimination
##   performance of multiple prognostic models at a specified follow-up
##   time point. For a given outcome, patients who experience the event
##   before or at the selected time point are defined as cases, while
##   patients who remain event-free beyond the selected time point are
##   defined as controls. Patients censored before the selected time point
##   are excluded from the ROC analysis.
##
## Main steps:
##   1. Define prognostic score variables from different models.
##   2. Construct a binary event status at the specified time point.
##   3. Exclude patients censored before the selected time point.
##   4. Generate ROC curves in the training cohort.
##   5. Generate ROC curves in the validation cohort.
##   6. Extract AUC values and 95% confidence intervals for each model.
##
## Arguments:
##   mydata       : Data frame containing follow-up time, outcome status,
##                  model-predicted prognostic scores, and Train/Test indices.
##   month_cutof  : Time point for ROC analysis, e.g., 12, 36, or 60 months.
##   edus         : Outcome variable name, such as "OS" or "DFS".
##
## Returns:
##   A list containing:
##     train_plot : ROC plot for the training cohort.
##     test_plot  : ROC plot for the validation cohort.
##     auc_table  : AUC values and 95% confidence intervals for all models.
##============================================================


roc_f <- function(mydata, month_cutof, edus){
  
  var_fp <- c("clinic", "baseline", "lctm", "fpca","clinebv", 
              "clinbase", "clinbaselctm", "clinbasepfcs","clinbaselctmpfc")
  
  tmp_data <- mydata
  tmp_index <- which(mydata$FollowUP > month_cutof)
  tmp_data[[edus]][tmp_index] <- 0
  tmp_index <- which(mydata[[edus]] == 1 | mydata$FollowUP >= month_cutof)
  
  # ---- 训练集 ROC ----
  roc_train <- roc(
    as.formula(paste0(edus,"~",paste(var_fp[1:9], collapse = "+"))),
    data = tmp_data[intersect(tmp_index, Index_train), ],
    auc = TRUE, ci = TRUE
  )
  
  g1 <- ggroc(roc_train, legacy.axes = TRUE, linewidth = 0.8) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                 color = "darkgrey", linetype = 4) +
    theme_bw() + coord_equal() +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
  
  # ---- 测试集 ROC ----
  roc_test <- roc(
    as.formula(paste0(edus,"~",paste(var_fp[1:9], collapse = "+"))),
    data = tmp_data[intersect(tmp_index, Index_test), ],
    auc = TRUE, ci = TRUE
  )
  
  g2 <- ggroc(roc_test, legacy.axes = TRUE, linewidth = 0.8) +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), 
                 color = "darkgrey", linetype = 4) +
    theme_bw() + coord_equal() +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
  
  # ---- 提取 AUC 和 95% CI ----
  auc_table <- data.frame(
    Model = var_fp[1:9],
    Train_AUC = sapply(var_fp[1:9], function(m) roc_train[[m]]$auc),
    Train_95CI = sapply(var_fp[1:9], function(m) paste0(
      round(roc_train[[m]]$ci[1], 2), " - ", round(roc_train[[m]]$ci[3], 2)
    )),
    Train_aci = sapply(var_fp[1:9], function(m) paste0(round(roc_train[[m]]$auc,2)," (", 
                                                       round(roc_train[[m]]$ci[1], 2), " - ", round(roc_train[[m]]$ci[3], 2), ")"
    )),
    Test_AUC = sapply(var_fp[1:9], function(m) roc_test[[m]]$auc),
    Test_95CI = sapply(var_fp[1:9], function(m) paste0(
      round(roc_test[[m]]$ci[1], 2), " - ", round(roc_test[[m]]$ci[3], 2)
    )),
    Test_aci = sapply(var_fp[1:9], function(m) paste0(round(roc_test[[m]]$auc,2)," (", 
                                                      round(roc_test[[m]]$ci[1], 2), " - ", round(roc_test[[m]]$ci[3], 2), ")"
    ))
  )
  
  return(list(train_plot = g1, test_plot = g2, auc_table = auc_table))
}