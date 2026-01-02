mydata <- df_baseline[Index_train,]
var_clin_base_lctmpfcs <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic, var_pfcs_aic)
num <- var_clin_base_lctmpfcs
mydata[num] <- lapply(mydata[num], as.numeric)

{train.x <- as.matrix(mydata[,var_clin_base_lctmpfcs])
  train.y <-ifelse(mydata$OS==1, mydata$FollowUP, -mydata$FollowUP)
  trainMat <- xgb.DMatrix(data = train.x, label = train.y)}

mydata <- df_baseline[Index_test,]
var_clin_base_lctmpfcs <- c(var_clinic_aic, var_baseline_aic, var_lctm_aic, var_pfcs_aic)
num <- var_clin_base_lctmpfcs
mydata[num] <- lapply(mydata[num], as.numeric)
{test.x <- as.matrix(mydata[,var_clin_base_lctmpfcs])
  test.y <-ifelse(mydata$OS==1, mydata$FollowUP, -mydata$FollowUP)
  testMat <- xgb.DMatrix(data = test.x, label = test.y)}
wlist = list(train = trainMat, test = testMat)
param<- list(objective ="survival:cox", booster="gbtree", eval_metric="cox-nloglik", 
             eta=0.03,max_depth=3, gamma=0.5) 
set.seed(123)
xgb.fit <- xgb.train(params = param, data = trainMat, nrounds =1000,
                     early_stopping_rounds=50, watchlist = wlist)


p_importance <- ggplot(
  shap_importance[1:20,],
  aes(x = reorder(Feature, Importance), y = Importance)
) +
  geom_col(fill = "#4682B4", width = 0.7, alpha = 0.9) +  # 柔和蓝色
  coord_flip() +
  labs(
    title = "特征重要性排序",
    subtitle = "基于平均 |SHAP| 值的特征贡献",
    x = "特征",
    y = "平均 |SHAP| 值"
  ) +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#333333", hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "#555555", hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 8)),
    axis.text = element_text(size = 12, color = "#333333"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray80", linetype = "dashed"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  ) +
  geom_text(
    aes(label = round(Importance, 3)),
    hjust = -0.2,
    size = 4,
    color = "#2F4F4F"
  ) +
  geom_col(aes(fill = Importance)) +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # 给标签留出空间


shap_long_norm <- shap_long %>%
  group_by(Feature) %>%
  mutate(Value_norm = (FeatureValue - min(FeatureValue, na.rm = TRUE)) / 
           (max(FeatureValue, na.rm = TRUE) - min(FeatureValue, na.rm = TRUE))) %>%
  ungroup()

# 若仅取前20重要特征
top_features <- shap_importance$Feature[1:20]
shap_long_top <- shap_long_norm %>%
  filter(Feature %in% top_features) %>%
  mutate(Feature = factor(Feature, levels = rev(top_features)))

# 绘图
p_summary <- ggplot(
  shap_long_top,
  aes(x = SHAP, y = Feature, color = Value_norm)  # 'value' 表示原始特征值（需要 shap_long 里有）
) +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed", linewidth = 0.8) +
  geom_jitter(height = 0.25, alpha = 0.7, size = 1.8) +  # 模拟蜂群散点
  scale_color_gradientn(
    colors = c("#2166AC", "#F7F7F7", "#B2182B"),  # 蓝 → 白 → 红
    name = "Value",
    guide = guide_colorbar(
      barwidth = 1.0, barheight = 10,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  scale_x_continuous(limits = c(-0.5, 1), expand = c(0.01, 0.01)) +
  labs(
    title = "SHAP",
    x = "SHAP）",
    y = NULL
  ) +
  theme_minimal(base_family = "Times", base_size = 14) +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 8)),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    plot.background = element_rect(fill = "white", color = "black", size = 0.8),
    panel.background = element_rect(fill = "white", color = "black", size = 0.8),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


p_summary

top_features <- shap_importance$Feature[c(2,3,4,5,6,7)]
dependence_plots <- lapply(top_features, function(feature_name) {
  feature_data <- shap_long %>%
    filter(Feature == feature_name)
  
  ggplot(feature_data, aes(x = FeatureValue, y = SHAP, color = FeatureValue)) +  
    geom_point(alpha = 0.7, size = 2) +                 # 增加点大小和透明度
    geom_smooth(method = "loess", color = "red", se = FALSE, size = 1) +  # LOESS曲线
    scale_color_viridis_c(option = "plasma") +          # 使用渐变色
    labs(
      title = paste("SHAP:", feature_name),
      x = "Feature Value",
      y = "SHAP Value",
      color = "Feature Value"
    ) +
    theme_minimal(base_family = "Times") +              # 设置字体
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
      
    )
})


combined_dependence <- wrap_plots(dependence_plots, ncol = 3)

