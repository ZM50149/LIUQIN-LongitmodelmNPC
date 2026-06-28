##============================================================
## Function: longt_fpca_train_valid_f
##
## Purpose:
##   This function performs FPCA for longitudinal biomarker trajectories
##   using a training-validation framework. The FPCA model is estimated
##   only in the training cohort, and validation trajectories are then
##   projected onto the FPCA basis derived from the training cohort.
## Arguments:
##   df_tmp:
##     Longitudinal data frame containing patient ID, time, and biomarker values.
##
##   varp:
##     Name of the biomarker or longitudinal variable, used for output file naming.
##
##   train_ids:
##     Patient IDs belonging to the training cohort.
##
##   test_ids:
##     Patient IDs belonging to the validation cohort.
##
##   main_path:
##     Main working directory for saving output figures.
##
## Returns:
##   A list containing:
##     train_ids_final:
##       Patient IDs retained in the training FPCA analysis.
##
##     test_ids_final:
##       Patient IDs retained in the validation FPCA projection.
##
##     time_grid:
##       Common time grid used for interpolation.
##
##     train_y_matrix:
##       Interpolated training trajectory matrix.
##
##     test_y_matrix:
##       Interpolated validation trajectory matrix.
##
##     train_xi:
##       FPCA scores estimated in the training cohort.
##
##     test_xi:
##       FPCA scores projected for the validation cohort.
##
##     fpca_result:
##       FPCA model object estimated using the training cohort.
##============================================================

longt_fpca_train_valid_f <- function(
    df_tmp,
    varp,
    train_ids,
    test_ids,
    main_path
) {
  time_grid <- seq(
    from = 0,
    to = 130,
    length.out = 50
  )
  make_y_matrix <- function(data_sub, time_grid) {
    
    longitudinal_df <- data_sub %>%
      group_by(Num) %>%
      filter(n() > 1) %>%
      ungroup()
    
    ptid_list <- unique(longitudinal_df$Num)
    
    y_matrix <- matrix(
      NA,
      nrow = length(ptid_list),
      ncol = length(time_grid)
    )
    for (i in seq_along(ptid_list)) {
      
      ptid_data <- longitudinal_df %>%
        filter(Num == ptid_list[i]) %>%
        arrange(TimeL)
      
      if (nrow(ptid_data) > 1) {
        
        approx_values <- approx(
          x      = ptid_data$TimeL,
          y      = ptid_data$longp,
          xout   = time_grid,
          method = "linear",
          rule   = 2
        )$y
        
        y_matrix[i, ] <- approx_values
      }
    }
    return(
      list(
        longitudinal_df = longitudinal_df,
        ptid_list       = ptid_list,
        y_matrix        = y_matrix
      )
    )
  }
  df_train <- df_tmp %>%
    filter(Num %in% train_ids)
  
  df_test <- df_tmp %>%
    filter(Num %in% test_ids)
  train_obj <- make_y_matrix(
    data_sub  = df_train,
    time_grid = time_grid
  )
  
  train_longitudinal_df <- train_obj$longitudinal_df
  train_ptid_list       <- train_obj$ptid_list
  train_y_matrix        <- train_obj$y_matrix
  train_fpca_fds <- fds(
    x     = time_grid,
    y     = t(train_y_matrix),
    xname = "TimeL",
    yname = "longp"
  )
  
  Ly_train <- split(
    t(train_fpca_fds$y),
    seq(ncol(train_fpca_fds$y))
  )
  
  Lt_train <- rep(
    list(train_fpca_fds$x),
    ncol(train_fpca_fds$y)
  )
  
  fpca_result <- FPCA(
    Ly = Ly_train,
    Lt = Lt_train,
    optns = list(
      methodMuCovEst = "smooth",
      userBwCov      = 15,
      FVEthreshold   = 0.95,
      verbose        = FALSE
    )
  )
  train_xi <- fpca_result$xiEst
    rownames(train_xi) <- train_ptid_list
  
  test_obj <- make_y_matrix(
    data_sub  = df_test,
    time_grid = time_grid
  )
  
  test_longitudinal_df <- test_obj$longitudinal_df
  test_ptid_list       <- test_obj$ptid_list
  test_y_matrix        <- test_obj$y_matrix
  train_mu  <- fpca_result$mu
  train_phi <- fpca_result$phi
  work_grid <- fpca_result$workGrid
    train_mu_grid <- approx(
    x    = work_grid,
    y    = train_mu,
    xout = time_grid,
    rule = 2
  )$y
    train_phi_grid <- apply(
    train_phi,
    2,
    function(z) {
      approx(
        x    = work_grid,
        y    = z,
        xout = time_grid,
        rule = 2
      )$y
    }
  )
    npc <- ncol(train_phi_grid)
  dt <- diff(time_grid)
  
  trap_weights <- c(
    dt[1] / 2,
    (dt[-1] + dt[-length(dt)]) / 2,
    dt[length(dt)] / 2
  )
  test_y_centered <- sweep(
    test_y_matrix,
    2,
    train_mu_grid,
    FUN = "-"
  )
  test_xi <- matrix(
    NA,
    nrow = nrow(test_y_centered),
    ncol = npc
  )
  
  for (k in seq_len(npc)) {
    
    test_xi[, k] <- test_y_centered %*% (
      train_phi_grid[, k] * trap_weights
    )
  }
  
  colnames(test_xi) <- paste0("PC", seq_len(npc))
  rownames(test_xi) <- test_ptid_list
  
  return(
    list(
      train_ids_final       = train_ptid_list,
      test_ids_final        = test_ptid_list,
      train_longitudinal_df = train_longitudinal_df,
      test_longitudinal_df  = test_longitudinal_df,
      time_grid             = time_grid,
      train_y_matrix        = train_y_matrix,
      test_y_matrix         = test_y_matrix,
      train_xi              = train_xi,
      test_xi               = test_xi,
      fpca_result           = fpca_result
    )
  )
}