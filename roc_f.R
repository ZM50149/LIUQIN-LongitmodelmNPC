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