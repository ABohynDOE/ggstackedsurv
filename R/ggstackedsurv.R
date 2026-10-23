#' Stacked survival plot for a multi-state survfit object
#'
#' Draws the state occupancy probabilities from a multi-state `survfit` object
#' as a stacked step-area plot. Add a risk table with [add_risktable()].
#'
#' @param x A multi-state `survfit` object, such as one from
#'   `survival::survfit()` with a factor `status` in `Surv(time, status)`.
#' @param theme A list of ggplot2 theme components added to the plot.
#'   Defaults to `theme_ggstackedsurv_default()`.
#' @param base_state A single string giving the name of the initial
#'   (no event yet) state. Defaults to `"Censored"`.
#'
#' @return A ggplot object.
#' @seealso [add_risktable()]
#' @export
ggstackedsurv <- function(
  x,
  theme = theme_ggstackedsurv_default(),
  base_state = "Censored"
) {
  if (!inherits(x, "survfit") || is.null(x$pstate)) {
    cli::cli_abort("{.arg x} must be a multi-state {.cls survfit} object.")
  }
  if (!is.character(base_state) || length(base_state) != 1) {
    cli::cli_abort("{.arg base_state} must be a single string.")
  }
  if (!is.null(x$strata)) {
    cli::cli_abort("Stratified {.cls survfit} objects are not yet supported.")
  }

  time <- x$time
  pstate <- x$pstate
  # Start the curves at time 0, when every subject is in the initial state.
  if (time[1] > 0) {
    time <- c(0, time)
    pstate <- rbind(c(1, numeric(ncol(pstate) - 1)), pstate)
  }
  # The initial state is the base (no event yet) level.
  colnames(pstate) <- c(base_state, x$states[-1])

  event_of_interest <- x$states[2]
  other_states <- setdiff(colnames(pstate)[-1], event_of_interest)
  # Time of first occupancy; states never occupied sort as latest.
  first_event <- vapply(
    other_states,
    function(s) {
      i <- which(pstate[, s] > 0)
      if (length(i)) time[i[1]] else Inf
    },
    numeric(1)
  )
  state_levels <- c(
    other_states[order(first_event, decreasing = TRUE)],
    base_state,
    event_of_interest
  )

  probs_long <- data.frame(
    x = rep(time, times = ncol(pstate)),
    y = as.vector(pstate),
    state = factor(
      rep(colnames(pstate), each = length(time)),
      levels = state_levels
    )
  )

  # Step each state separately so the curves hold until the next time.
  probs_step <- ggplot2:::dapply(probs_long, "state", ggplot2:::stairstep)

  p <- ggplot2::ggplot(probs_step, ggplot2::aes(x = x, y = y, fill = state)) +
    ggplot2::geom_area(color = "black", linewidth = 0.3) +
    ggplot2::labs(x = "time", y = "Probability") +
    ggplot2::coord_cartesian(xlim = range(time), expand = FALSE) +
    theme

  # Keep the fit so add_risktable() can compute table values from it.
  attr(p, "ggstackedsurv") <- list(survfit = x, states = colnames(pstate))
  class(p) <- c("ggstackedsurv", class(p))
  p
}
