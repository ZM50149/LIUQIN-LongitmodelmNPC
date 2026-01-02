Table_f <- function(mydata, sum_vars, Group){
  is_num <- sapply(mydata[, sum_vars, drop = FALSE], is.numeric)
  cont_vars   <- sum_vars[is_num]
  factorVars  <- sum_vars[!is_num]
  shapiro_p <- sapply(cont_vars, function(v) {
    x <- mydata[[v]]
    x <- x[!is.na(x)]
    if (length(unique(x)) < 3) return(NA_real_)
    shapiro.test(x)$p.value
  })
  norm_var <- names(shapiro_p)[!is.na(shapiro_p) & shapiro_p > 0.05]
  Result_Table_1 <- CreateTableOne(strata=Group, vars=sum_vars, factorVars  = factorVars, data=mydata, 
                                   test=TRUE, smd=TRUE, addOverall=TRUE)
  Tab1 <- print(Result_Table_1,
                showAllLevels = TRUE, catDigits=1, contDigits=1, pDigits=3, test=TRUE, smd=TRUE, noSpaces=TRUE,nonnormal = norm_var)
  
  stats_vec <- numeric(length(sum_vars))
  test_type <- character(length(sum_vars))
  names(stats_vec) <- sum_vars
  names(test_type) <- sum_vars
  
  for (v in sum_vars) {
    g <- mydata[[Group]]
    x <- mydata[[v]]
    
    keep <- !is.na(x) & !is.na(g)
    x <- x[keep]
    g <- droplevels(as.factor(g[keep]))
    
    # 跳过完全没数据或只有一组的情况
    if (length(x) == 0 || nlevels(g) < 2) {
      stats_vec[v] <- NA_real_
      test_type[v] <- NA_character_
      next
    }
    
    if (is.numeric(x)) {
      # 连续变量
      vals_by_group <- split(x, g)
      
      # 如果任意一组是恒量（或组内只有一个观测），不做 parametric 检验
      group_constant <- any(sapply(vals_by_group, function(z) length(unique(z)) < 2))
      
      if (nlevels(g) == 2) {
        test_type[v] <- "t"
        if (group_constant || length(unique(x)) < 2) {
          stats_vec[v] <- NA_real_
        } else {
          tmp <- tryCatch(
            t.test(x ~ g),
            error = function(e) NULL
          )
          stats_vec[v] <- if (!is.null(tmp)) unname(tmp$statistic) else NA_real_
        }
      } else {
        test_type[v] <- "F"
        if (group_constant || length(unique(x)) < 2) {
          stats_vec[v] <- NA_real_
        } else {
          tmp <- tryCatch(
            oneway.test(x ~ g),
            error = function(e) NULL
          )
          stats_vec[v] <- if (!is.null(tmp)) unname(tmp$statistic) else NA_real_
        }
      }
      
    } else {
      # 分类变量：卡方检验
      tab <- table(x, g)
      test_type[v] <- "χ²"
      
      if (all(dim(tab) > 1)) {
        tmp <- suppressWarnings(
          tryCatch(chisq.test(tab), error = function(e) NULL)
        )
        stats_vec[v] <- if (!is.null(tmp)) unname(tmp$statistic) else NA_real_
      } else {
        stats_vec[v] <- NA_real_
      }
    }
  }
  
  # 5. 把统计量并入 Tab1
  Tab1 <- as.data.frame(Tab1)
  rn <- rownames(Tab1)
  
  # 从行名中截取变量名：遇到 ".." 或 空格 之后的内容都去掉
  row_var <- sub("(\\.\\.|\\s).*", "", rn)
  row_var <- trimws(row_var)  # 去掉可能的首尾空格
  Tab1$Statistic <- NA_real_
  Tab1$Test      <- NA_character_
  for (v in sum_vars) {
    idx <- which(row_var == v)[1]  # 只在该变量的第一行填入统计量
    if (length(idx) == 1 && !is.na(stats_vec[v])) {
      Tab1$Statistic[idx] <- round(stats_vec[v], 3)
      Tab1$Test[idx]      <- test_type[v]
    }
  }
  return(Tab1)
  
}