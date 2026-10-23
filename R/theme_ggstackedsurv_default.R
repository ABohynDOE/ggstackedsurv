theme_ggstackedsurv_default <- function() {
  list(
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = 0, add = 0)
    ),
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = 0, add = 0)
    ),
    ggplot2::theme_bw(),
    ggplot2::theme(legend.position = "top"),
    ggplot2::theme(legend.title = ggplot2::element_blank()),
    ggplot2::theme(strip.background = ggplot2::element_blank()),
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10)),
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12)),
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10)),
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = NA,
        colour = "black"
      )
    )
  )
}
