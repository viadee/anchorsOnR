#' Plot a condensed overview of all explanations
#'
#' This function produces a facetted heatmap visualisation of all
#' case/label/feature combinations. Compared to [plot_features()] it is much
#' more condensed, thus allowing for an overview of many explanations in one
#' plot. On the other hand it is less useful for getting exact numerical
#' statistics of the explanation.
#'
#' @param explanation A `data.frame` as returned by [explain()].
#' @param ... Parameters passed on to [ggplot2::facet_wrap()]
#'
#' @return A `ggplot` object
#'
#' @import ggplot2
#' @export
#'
#' @family explanation plots
plotExplanations <- function(explanation, ...) {
  num_cases <- unique(suppressWarnings(as.numeric(explanation$case)))
  if (!anyNA(num_cases)) {
    explanation$case <- factor(explanation$case, levels = as.character(sort(num_cases)))
  }
  explanation$feature_desc <- factor(
    explanation$feature_desc,
    levels = rev(unique(explanation$feature_desc[order(explanation$feature, explanation$feature_value)]))
  )
  p <- ggplot(explanation, aes_(~case, ~feature_desc)) +
    geom_tile(aes_(fill = ~feature_weight)) +
    scale_x_discrete('Case', expand = c(0, 0)) +
    scale_y_discrete('Feature', expand = c(0, 0)) +
    scale_fill_gradient2('Feature\nweight', low = 'firebrick', mid = '#f7f7f7', high = 'steelblue') +
    theme_lime() +
    theme(panel.border = element_rect(fill = NA, colour = 'grey60', size = 1),
          panel.grid = element_blank(),
          legend.position = 'right',
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  if (is.null(explanation$label)) {
    p
  } else {
    p + facet_wrap(~label, ...)
  }
}

theme_lime <- function(...) {
  theme_minimal() +
    theme(
      strip.text = element_text(face = 'bold', size = 9),
      plot.margin = margin(15, 15, 15, 15),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      panel.spacing.y = unit(15, 'pt'),
      strip.text.x = element_text(margin = margin(t = 2, b = 2), hjust = 0),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      ...
    )
}
