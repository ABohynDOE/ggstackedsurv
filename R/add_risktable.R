#' Add a risk table
#'
#' Add a risk table below a [ggstackedsurv()] plot showing, at chosen times,
#' the number at risk, the number of events and the number of censored
#' observations.
#'
#' `add_risktable()` does not draw anything by itself. It stores its settings
#' on the plot, and the table is built when the plot is printed or saved (see
#' [ggstackedsurv_build()]). The table is a separate ggplot sharing the x
#' scale of the main plot, stacked underneath with patchwork, so it stays
#' aligned whatever you change on the main plot.
#'
#' @section What the statistics mean:
#' The statistics describe the subjects who have not yet left the initial
#' state of the multi-state model, i.e. the `base_state` of
#' [ggstackedsurv()].
#' * `"n.risk"`: number of subjects at risk (in the initial state).
#' * `"n.event"`: number of transitions out of the initial state in the
#'   interval since the previous time shown in the table.
#' * `"cum.event"`: cumulative number of transitions out of the initial state.
#' * `"n.censor"`: number of censored observations in the interval.
#' * `"cum.censor"`: cumulative number of censored observations.
#'
#' @section Customize statistics:
#' `risktable_stats` accepts [glue][glue::glue()]-like syntax. For example
#' `risktable_stats = "{n.risk} ({cum.event})"` puts the number at risk and the
#' number of events in the same row.
#'
#' @param times Numeric vector of times where the risk table values are
#'   placed. Default are the breaks shown on the x-axis. This does not change
#'   the tick marks of the figure, use `ggplot2::scale_x_continuous(breaks =)`
#'   for that.
#' @param risktable_stats Character vector of statistics to show, one row per
#'   element. Elements of `c("n.risk", "n.event", "cum.event", "n.censor",
#'   "cum.censor")` or glue strings combining them. Default is
#'   `c("n.risk", "cum.event")`.
#' @param stats_label Named vector or list of custom row labels. The names are
#'   the elements of `risktable_stats` and the values the labels. An unnamed
#'   vector of the same length as `risktable_stats` is used in order.
#' @param risktable_height A number between 0 and 1: the proportion of the
#'   final figure occupied by the risk table. Default is chosen from the
#'   number of rows.
#' @param theme A risk table theme. Default is [theme_risktable_default()].
#' @param size,... Arguments passed to [ggplot2::geom_text()], for example
#'   `size = 4` to enlarge the numbers.
#'
#' @return An object that can be added to a [ggstackedsurv()] plot with `+`.
#' @seealso [ggstackedsurv()], [ggstackedsurv_build()]
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' fit <- survival::survfit(
#'   survival::Surv(time, status) ~ 1,
#'   data = ecmo
#' )
#' p <- ggstackedsurv(fit)
#'
#' p + add_risktable()
#'
#' p + add_risktable(risktable_stats = "{n.risk} ({cum.event})")
#'
#' p + add_risktable(
#'   risktable_stats = c("n.risk", "cum.event"),
#'   stats_label = c(n.risk = "Number at risk", cum.event = "Events so far")
#' )
#' @export
add_risktable <- function(
  times = NULL,
  risktable_stats = c("n.risk", "cum.event"),
  stats_label = NULL,
  risktable_height = NULL,
  theme = theme_risktable_default(),
  size = 3.5,
  ...
) {
  if (!is.null(times) && (!is.numeric(times) || anyNA(times))) {
    cli::cli_abort("{.arg times} must be a numeric vector without missing values.")
  }
  if (!is.character(risktable_stats) || length(risktable_stats) == 0) {
    cli::cli_abort("{.arg risktable_stats} must be a character vector.")
  }
  if (
    !is.null(risktable_height) &&
      (!is.numeric(risktable_height) ||
        length(risktable_height) != 1 ||
        risktable_height <= 0 ||
        risktable_height >= 1)
  ) {
    cli::cli_abort(
      "{.arg risktable_height} must be a single number between 0 and 1."
    )
  }

  settings <- list(
    times = times,
    risktable_stats_raw = risktable_stats,
    risktable_stats = .convert_to_glue(risktable_stats),
    stats_label = stats_label,
    risktable_height = risktable_height,
    theme = theme,
    text_args = utils::modifyList(list(size = size), list(...))
  )

  # The class and attribute names differ from ggsurvfit on purpose, so the two
  # packages can be loaded together without their methods clashing.
  structure(
    list(),
    ggstackedsurv_risktable = settings,
    class = "ggstackedsurv_add_risktable"
  )
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggstackedsurv_add_risktable <- function(object, plot, object_name) {
  if (!inherits(plot, "ggstackedsurv")) {
    cli::cli_abort(
      "{.fn add_risktable} can only be added to a {.fn ggstackedsurv} plot."
    )
  }
  settings <- attr(object, "ggstackedsurv_risktable")

  # An empty layer carries the settings along with the plot. The table itself
  # is built at print time, once the final scales of the plot are known.
  carrier <- structure(
    ggplot2::geom_blank(),
    ggstackedsurv_risktable = settings
  )

  plot +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(5.5, 5.5, 0, 5.5), "points"),
      legend.margin = ggplot2::margin(t = 0, b = 0)
    ) +
    carrier
}

# Wrap the known statistics in braces so that plain names work with glue.
.convert_to_glue <- function(x) {
  possible_stats <- c("n.risk", "n.event", "n.censor", "cum.event", "cum.censor")
  x[x %in% possible_stats] <- paste0("{", x[x %in% possible_stats], "}")
  x
}
