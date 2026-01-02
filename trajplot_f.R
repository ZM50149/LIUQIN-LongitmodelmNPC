# for FPCA
trajplot_f <- function(df_baseline,longitudinal_df,ptid_list,y_matrix,fpca_result,varp){
  selected_rows <- df_baseline %>%
    filter(Num %in% longitudinal_df$Num)
  plot_df <- data.frame(
    PTID = rep(ptid_list, each = length(time_grid)),
    time = rep(time_grid, times = length(ptid_list)),
    value = as.vector(t(y_matrix)),
    group = rep(selected_rows$OS, each = length(time_grid)))
  
  fac <- c("group")
  plot_df[fac] <- lapply(plot_df[fac], as.factor)
  
  mean_function <- data.frame(
    time = fpca_result$workGrid,
    value = fpca_result$mu,
    type = "MeanFunction")
  
  phi_funcs <- data.frame(
    time = rep(fpca_result$workGrid, 2),
    value = c(fpca_result$phi[,1], fpca_result$phi[,2]),
    component = factor(rep(c("1st FPC", "2nd FPC"), each=length(fpca_result$workGrid))))
  
  y_primary_range <- range(plot_df$value, na.rm = TRUE)
  y_secondary_range <- range(phi_funcs$value)
  
  scale_factor <- diff(y_primary_range)/diff(y_secondary_range)
  shift_value <- y_primary_range[1] - y_secondary_range[1] * scale_factor
  
  phi_funcs$transformed_value <- phi_funcs$value * scale_factor + shift_value
  
  p <- ggplot(plot_df, aes(x = time, y = value, color = group)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"))+
    theme_bw()+
    scale_x_continuous(limits = c(0,126), breaks = seq(0, 126, 21), expand=c(0,0))+
    scale_color_manual(values = c("#3E608D","#4E1945","#CB9475"))+
    theme(text=element_text(family="serif"),
          axis.text=element_text(size=20))
  
  ggsave(filename =file.path(main_path, paste0("Results/Trajectory_class/",varp, ".pdf")),
         plot = p, width = 10, height = 6,bg = "white")
}



# for LCTM
trajplot_f <- function(df_baseline,longitudinal_df,ptid_list,y_matrix,varp){
  selected_rows <- df_baseline %>%
    filter(Num %in% longitudinal_df$Num)
  plot_df <- data.frame(
    PTID = rep(ptid_list, each = length(time_grid)),
    time = rep(time_grid, times = length(ptid_list)),
    value = as.vector(t(y_matrix)),
    group = rep(selected_rows[[paste0(varp, "class")]], each = length(time_grid)))
  
  fac <- c("group")
  plot_df[fac] <- lapply(plot_df[fac], as.factor)
  
  p <- ggplot(plot_df, aes(x = time, y = value, color = group)) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"))+
    theme_bw()+
    scale_x_continuous(limits = c(0,126), breaks = seq(0, 126, 21), expand=c(0,0))+
    scale_color_manual(values = c("#3E608D","#4E1945","#CB9475","#F09BA0"))+
    theme(text=element_text(family="serif"),
          axis.text=element_text(size=20))
  
  ggsave(filename =file.path(main_path, paste0("Results/Trajectory_class/",varp, ".pdf")),
         plot = p, width = 10, height = 6,bg = "white")
}