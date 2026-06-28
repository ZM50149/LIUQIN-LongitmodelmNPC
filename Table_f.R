##============================================================
## Function: Table_f
##
## Purpose:
##   This function generates a baseline characteristics table
##   stratified by a specified grouping variable. It summarizes
##   continuous and categorical variables, performs between-group
##   comparisons, calculates standardized mean differences, and
##   additionally extracts test statistics for each variable.
##
## Main steps:
##   1. Identify continuous and categorical variables.
##   2. Assess normality of continuous variables using the Shapiro-Wilk test.
##   3. Generate a descriptive table using CreateTableOne().
##   4. Calculate test statistics for each variable:
##        - t statistic for continuous variables with two groups.
##        - F statistic for continuous variables with more than two groups.
##        - χ² statistic for categorical variables.
##   5. Merge test statistics and test types into the final table.
##
## Arguments:
##   mydata   : Data frame containing variables for analysis.
##   sum_vars : Character vector of variables to be summarized.
##   Group    : Grouping variable used for stratification.
##
## Returns:
##   A data frame containing descriptive statistics, P values,
##   standardized mean differences, test statistics, and test types.
##============================================================


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
    if (length(x) == 0 || nlevels(g) < 2) {
      stats_vec[v] <- NA_real_
      test_type[v] <- NA_character_
      next
    }
    
    if (is.numeric(x)) {
      vals_by_group <- split(x, g)
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
  
  Tab1 <- as.data.frame(Tab1)
  rn <- rownames(Tab1)

  row_var <- sub("(\\.\\.|\\s).*", "", rn)
  row_var <- trimws(row_var) 
  Tab1$Statistic <- NA_real_
  Tab1$Test      <- NA_character_
  for (v in sum_vars) {
    idx <- which(row_var == v)[1]  
    if (length(idx) == 1 && !is.na(stats_vec[v])) {
      Tab1$Statistic[idx] <- round(stats_vec[v], 3)
      Tab1$Test[idx]      <- test_type[v]
    }
  }
  return(Tab1)
  
}