##============================================================
## Training-Validation Framework for Latent Class Mixed Models
##
## Purpose:
##   This code fits a latent class mixed model (LCMM/HLME) for a
##   longitudinal biomarker trajectory using the training cohort only.
##   The fitted trajectory model is then applied to the validation cohort
##   to assign each validation patient to the most likely trajectory class.
##
## Main steps:
##   1. Define data sets.
##   2. Fit a one-class mixed model.
##   4. Fit a multi-class latent class mixed model.
##   5. Predict class-specific longitudinal trajectories from the training model.
##   6. Extract posterior class membership for the training cohort.
##   7. Assign validation patients to trajectory classes using the trained model.
##============================================================


##============================================================
## 1. Define data sets
##============================================================
train_ids <- df_baseline$Num[Index_train]
test_ids  <- df_baseline$Num[Index_test]

df_follow_train <- df_follow %>%
  filter(Num %in% train_ids)

df_follow_test <- df_follow %>%
  filter(Num %in% test_ids)

##============================================================
## 2. Fit one-class mixed model in the training cohort
##============================================================
m1_train <- hlme(
  fixed = dNLR ~ TimeL + I(TimeL^2),
  random = ~ TimeL,
  ng = 1,
  data = df_follow_train,
  subject = "Num"
)

##============================================================
## 3. Fit multi-class latent class mixed model in the training cohort
##============================================================
m_train <- hlme(
  fixed   = dNLR ~ TimeL + I(TimeL^2),
  mixture = ~ TimeL, random  = ~ TimeL,
  ng = 3,
  data    = df_follow_train, subject = "Num", B = m1_train
)

##============================================================
## 4. Predict class-specific trajectories from the training model
##============================================================
newdata <- data.frame(TimeL = seq(0, 180, length = 100))
plotpred_train <- predictY(
  m_train, newdata, var.time = "TimeL",
  draws    = TRUE
)

##============================================================
## 6. Extract posterior class membership in the training cohort
##============================================================
train_class <- m_train$pprob
colnames(train_class)[1] <- "Num"
train_class <- train_class %>%
  mutate(Cohort = "Training")

##============================================================
## 7. Assign validation patients to trajectory classes
##============================================================
valid_class <- predictClass(m_train, newdata = df_follow_test, subject = "Num")
colnames(valid_class)[1] <- "Num"

valid_class <- valid_class %>% mutate(Cohort = "Validation")
class_all <- bind_rows(train_class,valid_class)
