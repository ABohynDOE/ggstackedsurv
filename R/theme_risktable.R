#' Default theme of the risk table
#'
#' A list of ggplot2 theme components used by [add_risktable()]. Pass your own
#' list of components to `add_risktable(theme = )` to change the look.
#'
#' @param axis.text.y.size Size of the row labels.
#' @param plot.title.size Size of the title.
#'
#' @return A list of ggplot2 theme components.
#' @seealso [add_risktable()]
#' @export
theme_risktable_default <- function(
  axis.text.y.size = 10,
  plot.title.size = 10.75
) {
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 9, vjust = 1, hjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = axis.text.y.size,
        colour = "black",
        face = "plain",
        # Keeps the labels clear of the numbers printed at the left edge.
        margin = ggplot2::margin(r = 14)
      ),
      plot.margin = ggplot2::unit(c(0, 5.5, 0, 5.5), "points"),
      plot.title = ggplot2::element_text(
        hjust = 0,
        vjust = 0,
        size = plot.title.size
      ),
      legend.position = "none",
    ),
    ggplot2::xlab(NULL),
    ggplot2::ylab(NULL)
  )
}
