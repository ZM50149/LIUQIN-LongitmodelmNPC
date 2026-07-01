##============================================================
## Machine Learning-Based Prognostic Signature Development
## and Model Performance Visualization
##
## Purpose:
##   This code prepares the training and validation datasets for
##   machine learning-based survival model development, sources the
##   customized ML.Dev.Prog.Sig_modified() function, fits multiple
##   prognostic models using candidate variables, and visualizes the
##   C-index performance across training and validation datasets.
##
## Main steps:
##   1. Load the customized machine learning survival modeling function.
##   2. Define candidate prognostic variables and Prepare the modeling dataset
##   3. Run machine learning-based prognostic model development.
##   4. Display C-index performance across datasets.
##============================================================


##============================================================
## 1. Load customized machine learning survival modeling function
##============================================================
source(
  file.path(
    main_path,
    "code/my_ML.Dev.Prog.Sig_modified_functions.R"
  )
)


##============================================================
## 2. Define candidate prognostic variables and prepare modeling dataset
##============================================================
var <- c(
  var_clinic_aic,
  var_baseline_aic,
  var_pfcs_aic,
  var_lctm_aic
)


mydata <- df_baseline[
  ,
  c("Num", "FollowUP", "OS", var)
]

colnames(mydata) <- c(
  "ID",
  "OS.time",
  "OS",
  var
)

mydata <- data.frame(mydata)


list_train_vali_Data <- list(
  Dataset1 = mydata[Index_train, ],
  Dataset2 = mydata[-Index_train, ]
)


##============================================================
## 3. Run machine learning-based prognostic model development
##============================================================
## ML.Dev.Prog.Sig_modified() evaluates multiple survival machine
## learning algorithms and algorithm combinations when mode = "all".
## Key parameters:
##   train_data           : training dataset.
##   list_train_vali_Data : list containing training and validation datasets.
##   candidate_genes      : candidate prognostic predictors.
##   mode = "all"         : run all supported modeling strategies.
##   nodesize = 5         : terminal node size for random survival forest.
##   seed = 1             : random seed for reproducibility.
##
## The returned object generally includes:
##   Cindex.res : C-index performance of each model across datasets.
##   ml.res     : fitted model objects.
##   riskscore  : predicted risk scores for each dataset.
##   Sig.genes  : final candidate predictors used in modeling.

res <- ML.Dev.Prog.Sig_modified(
  train_data           = list_train_vali_Data$Dataset1,
  list_train_vali_Data = list_train_vali_Data,
  candidate_genes      = var,
  mode                 = "all",
  nodesize             = 5,
  seed                 = 1
)


##============================================================
## 4. Visualize C-index performance across datasets
##============================================================
cindex_dis_all(
  res,
  validate_set = names(list_train_vali_Data)[-1],
  order        = names(list_train_vali_Data),
  width        = 0.35
)