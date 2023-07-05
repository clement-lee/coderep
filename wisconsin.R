## ----prelim, message = FALSE--------------------------------
library(tibble)
library(dplyr)
library(ggplot2)
library(caret)
library(ROCR)
library(pROC)
source("plots.R")
source("models.R")
theme_set(theme_bw(12))


## ----read---------------------------------------------------
cancer_data <- as_tibble(read.csv("data/breast-cancer-wisconsin.csv"))
head(cancer_data)
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)
colnames(cancer_data)
dim(cancer_data)


## ----remove_na----------------------------------------------
cancer_data <- cancer_data |> select(where(~ all(!is.na(.x))))
head(cancer_data)


## ----plot-counts, fig.cap = "Counts of data according to tumour status", fig.align = "center", out.width = "70%", fig.asp = 0.65----
plot_count(cancer_data)


## ----plot-hist, fig.show = "hold", fig.cap = "Histogram and density of three features according to tumour status", fig.align = "center", out.width = "70%", fig.asp = 0.65----
plot_hist_den(cancer_data, var = area_worst, label = "Area worst")
plot_hist_den(cancer_data, var = fractal_dimension_mean, label = "Fractal dimension mean")
plot_hist_den(cancer_data, var = radius_se, label = "Radius se")


## ----plot-boxplot, fig.cap = "Boxplot of three features according to tumour status", fig.show = "hold", fig.pos = "!h", fig.align = "center", out.width = "70%", fig.asp = 0.65----
plot_boxplot(cancer_data, var = area_worst, label = "Area worst")
plot_boxplot(cancer_data, var = fractal_dimension_mean, label = "Fractal dimension mean")
plot_boxplot(cancer_data, var = radius_se, label = "Radius se")


## ----plot-smoothed, warning = FALSE, message = FALSE, fig.cap = "Tumour status against each feature with fitted logistic regression line", fig.show = "hold", fig.pos = "!h", fig.asp = 0.5, fig.align = "center", out.width = "70%", fig.asp = 0.65----
plot_logistic_smoothed(cancer_data, var = area_worst)
plot_logistic_smoothed(cancer_data, var = fractal_dimension_mean)
plot_logistic_smoothed(cancer_data, var = radius_se)


## ----ttest, results = "hold"--------------------------------
area_worst_B <- cancer_data$area_worst[cancer_data$diagnosis == "B"]
area_worst_M <- cancer_data$area_worst[cancer_data$diagnosis == "M"]
ttest0 <- t.test(area_worst_B, area_worst_M, var.equal = TRUE)
options(scipen = 3, digits = 3)


## ----ttest_functional---------------------------------------
ttest1 <- ttest_var(cancer_data, var = area_worst)
ttest2 <- ttest_var(cancer_data, var = fractal_dimension_mean)
ttest3 <- ttest_var(cancer_data, var = radius_se)


## ----drop, results = "hold"---------------------------------
input_data <- cancer_data |> select(-id, -diagnosis)


## ----plot-corr, fig.cap = "Correlation matrix heatmap", out.width = "80%", fig.align = "center", fig.asp = 0.65----
plot_heatmap(input_data)


## ----drop_corr----------------------------------------------
correlation_data <- remove_cols_high_cor(input_data)
names(correlation_data)


## ----cross_validation, warning = FALSE----------------------
list_model <- create_model(cancer_data)
pred_y <- list_model$pred
cancer_test <- list_model$test


## ----confusion_matrix---------------------------------------
cm <- confusionMatrix(pred_y, cancer_test$diagnosis, positive = 'M')
cm
TN <- cm$table[1,1]
TP <- cm$table[2,2]
FN <- cm$table[2,1]
FP <- cm$table[1,2]


## ----classification_report----------------------------------
sensitivity(pred_y, cancer_test$diagnosis, positive = "M")
specificity(pred_y, cancer_test$diagnosis, positive = "M")
posPredValue(pred_y, cancer_test$diagnosis, positive = "M")
negPredValue(pred_y, cancer_test$diagnosis, positive = "M")
precision(pred_y, cancer_test$diagnosis, positive = "M")
recall(pred_y, cancer_test$diagnosis, positive = "M")


## ----auc----------------------------------------------------
pred_y_num <- as.numeric(pred_y)
test_diagnosis_num <- as.numeric(cancer_test$diagnosis)
auc_cancer <- auc(pred_y_num, test_diagnosis_num)


## ----roc, fig.cap = "ROC curve for the logistic regression model. Source: Wisconsin Breast Cancer Dataset.", fig.align = "center", out.width = "70%", fig.asp = 0.65----
plot_roc(pred_y, cancer_test$diagnosis)


## ----session_info-------------------------------------------
sessionInfo()

