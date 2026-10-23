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
        face = "plain"
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
