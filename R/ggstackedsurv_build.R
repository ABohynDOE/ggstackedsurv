#' Build a ggstackedsurv plot
#'
#' Combines a [ggstackedsurv()] plot with the risk table added by
#' [add_risktable()], if any. This is what happens when a `ggstackedsurv` plot
#' is printed or saved with [ggplot2::ggsave()]. Call it directly to get the
#' pieces and arrange them yourself.
#'
#' @param x A plot created by [ggstackedsurv()].
#' @param combine_plots Logical. If `TRUE` (default), the plot and the risk
#'   table are stacked into one patchwork figure. If `FALSE`, a list with
#'   elements `plot` and `table` is returned (`table` is `NULL` when there is
#'   no risk table).
#'
#' @return A ggplot/patchwork object, or a list of ggplots when
#'   `combine_plots = FALSE`.
#' @seealso [add_risktable()]
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = ecmo)
#' p <- ggstackedsurv(fit) + add_risktable()
#' parts <- ggstackedsurv_build(p, combine_plots = FALSE)
#' names(parts)
#' @export
ggstackedsurv_build <- function(x, combine_plots = TRUE) {
  if (!inherits(x, "ggstackedsurv")) {
    cli::cli_abort("{.arg x} must be a plot created by {.fn ggstackedsurv}.")
  }

  settings <- .get_risktable_settings(x)

  # Drop our class, otherwise printing the result would call this function
  # again.
  p <- x
  class(p) <- setdiff(class(p), "ggstackedsurv")

  if (is.null(settings)) {
    if (combine_plots) {
      return(p)
    }
    return(list(plot = p, table = NULL))
  }

  fit <- attr(x, "ggstackedsurv")$survfit

  # Read the final x range and breaks from the built plot, so the table
  # follows any scale the user has changed.
  panel <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  xlim <- panel$x.range
  times <- settings$times
  if (is.null(times)) {
    times <- panel$x$get_breaks()
  }
  times <- times[is.finite(times)]
  if (length(times) == 0) {
    cli::cli_abort("No times available for the risk table.")
  }

  stats <- .risktable_stats(fit, times)
  tbl <- .construct_risktable(.risktable_data(stats, settings), xlim, settings)

  if (!combine_plots) {
    return(list(plot = p, table = tbl))
  }

  height <- settings$risktable_height
  if (is.null(height)) {
    n_rows <- length(settings$risktable_stats)
    height <- min(0.5, 0.06 + 0.05 * n_rows)
  }
  patchwork::wrap_plots(p, tbl, ncol = 1, heights = c(1 - height, height))
}

# Settings of the last risk table added to the plot, or NULL.
.get_risktable_settings <- function(x) {
  found <- Filter(
    Negate(is.null),
    lapply(x$layers, function(l) attr(l, "ggstackedsurv_risktable"))
  )
  if (length(found) == 0) {
    return(NULL)
  }
  found[[length(found)]]
}

#' @export
print.ggstackedsurv <- function(x, ...) {
  print(ggstackedsurv_build(x, combine_plots = TRUE), ...)
  invisible(x)
}

# ggsave() draws plots with grid::grid.draw(), which would skip the table.
#' @exportS3Method grid::grid.draw
grid.draw.ggstackedsurv <- function(x, recording = TRUE) {
  grid::grid.draw(ggstackedsurv_build(x, combine_plots = TRUE), recording = recording)
}
