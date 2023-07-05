#' 2-sample t test of a variable
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @param var A variable name (without quotes).
#' @return A list with class "htest".
ttest_var <- function(df, var) {
  var0 <- deparse(substitute(var))
  vec0 <- df[[var0]]
  vec_B <- vec0[df$diagnosis == "B"]
  vec_M <- vec0[df$diagnosis == "M"]
  t.test(vec_B, vec_M, var.equal = TRUE)
}

#' remove columns of high correlation
#'
#' @param df A data frame.
#' @param threshold Level columns with correlation above which will be removed.
#' @return A data frame with removed columns.
remove_cols_high_cor <- function(df, threshold = 0.95) {
  corr_matrix <- cor(df)
  corr_matrix[!upper.tri(corr_matrix)] <- as.numeric(NA)
  features_to_omit <- unique(which(corr_matrix > threshold, arr.ind = TRUE)[, "col"])
  input_data |> dplyr::select(-tidyselect::all_of(features_to_omit))
}

#' obtain training results with logistic regression
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @return A list of length 2: `pred` is the response values predicted by logistic regression, `test` is the testing data
create_model <- function(df, seed = 1234L) {
  set.seed(seed)
  df_index <-
    caret::createDataPartition(
      df$diagnosis,
      times = 1,
      p = 0.80,
      list = FALSE
    )
  df_train <- df[df_index, ]
  df_test <- df[-df_index, ]
  tr_control <-
    trainControl(
      method = "cv",
      number = 15,
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary
    )
  logRegress <-
    caret::train(
      diagnosis ~ .,
      data = df,
      method = 'glm',
      metric = 'ROC',
      preProcess = c('scale', 'center'),
      trControl = tr_control,
      maxit = 100
    )
  list(pred = predict(logRegress, dplyr::select(df_test, -diagnosis)), test = df_test)
}
