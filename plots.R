#' plot histogram overlaid with density
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @param var A variable name (without quotes).
#' @param label A literate string for var.
#' @return A ggplot object.
plot_hist_den <- function(df, var, label, filename = "histogram.png") {
  if (!("diagnosis" %in% names(df))) {
    stop("df has to contain a variable named 'diagnosis'.")
  }
  gg <- df |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes({{ var }}, y = ..density.., fill = diagnosis), alpha = 0.5, bins = 40
    ) +
    ggplot2::geom_density(aes({{ var }}, col = diagnosis), lwd = 2) +
    ggplot2::labs(title = paste0("Distribution of ", label), x = label)
  ggsave(filename, gg) # save before output
  gg
}

#' plot boxplot according to diagnosis
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @param var A variable name (without quotes).
#' @param label A literate string for var.
#' @return A ggplot object.
plot_boxplot <- function(df, var, label, filename = "boxplot.png") {
  if (!("diagnosis" %in% names(df))) {
    stop("df has to contain a variable named 'diagnosis'.")
  }
  gg <- df |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(diagnosis, {{ var }})) +
    ggplot2::labs(x = label)
  ggsave(filename, gg) # save before output
  gg
}

#' plot binary data with fitted logistic regression line
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @param var A variable name (without quotes).
#' @return A ggplot object.
plot_logistic_smoothed <- function(df, var, filename = "smoothed.png") {
  if (!("diagnosis" %in% names(df))) {
    stop("df has to contain a variable named 'diagnosis'.")
  }
  gg <- df |>
    ggplot2::ggplot(ggplot2::aes({{ var }}, as.numeric(diagnosis) - 1.0)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    ggplot2::labs(y = "Probability")
  ggsave(filename, gg) # save before output
  gg
}

#' plot correlation matrix heatmap
#'
#' @param df A data frame.
#' @return A ggplot object.
plot_heatmap <- function(df, filename = "heatmap.png") {
  corr_matrix <- cor(df)
  corr_matrix[!lower.tri(corr_matrix)] <- as.numeric(NA)
  corr_df <- corr_matrix |> reshape2::melt() |> tibble::as_tibble() |> filter(!is.na(value))
  gg <- corr_df |> 
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::scale_fill_gradient2(
      low = "blue", high = "red", mid = "grey100", 
      midpoint = 0.5, space = "Lab", 
      name = "Pearson\nCorrelation"
    ) +
    ggplot2::geom_text(ggplot2::aes(Var2, Var1, label = round(value, 2)), color = "black", size = 0.9) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, size = 8, hjust = 1),
      axis.text.y = ggplot2::element_text(hjust = 1, size = 8)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::scale_y_discrete(limits = rev(levels(corr_df$Var1)))
  ggsave(filename, gg) # save before output
  gg
}

#' plot counts
#'
#' @param df A data frame with at least two variables, including one named diagnosis.
#' @return A ggplot object.
plot_count <- function(df, filename = "count.png") {
  if (!("diagnosis" %in% names(df))) {
    stop("df has to contain a variable named 'diagnosis'.")
  }
  gg <- df |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(diagnosis, fill = diagnosis)) +
    ggplot2::theme_bw(12) +
    ggplot2::labs(title = "A count of benign and malignant tumours")
  ggsave(filename, gg)
  gg
}

#' create receiver operating characteristics (ROC) curve
#'
#' @param pred predicted values
#' @param test actual values in test data
#' @return A ggplot object for the ROC curve.
plot_roc <- function(pred, test, filename = "roc.png") {
  pred <- as.numeric(pred)
  test <- as.numeric(test)
  auc0 <- pROC::auc(pred, test)
  pred_roc <- ROCR::prediction(pred, test)
  roc_perf <- ROCR::performance(pred_roc, measure = "tpr", x.measure = "fpr")
  roc_obj <- pROC::roc(pred, test)
  ci_auc <- pROC::ci.auc(roc_obj)
  ci_lower <- round(ci_auc[1], 2)
  ci_upper <- round(ci_auc[3], 2)
  label_annotate <-
    glue::glue("AUC = {round(auc0, 2)} with 95% CI = ({ci_lower} - {ci_upper})")
  gg <-
    ggroc(roc_obj, colour = "lightblue", size = 2, legacy.axes = TRUE) +
    geom_segment(
      aes(x = 0, xend = 1, y = 0, yend = 1),
      colour = "grey",
      linetype = "dashed"
    ) +
    annotate("text", x = 0.7, y = 0.05, label = label_annotate) +
    labs(
      title = "ROC Curve",
      subtitle = paste0("(AUC = ", auc0 |> round(4), ")"),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  ggsave(filename, gg)
  gg
}
