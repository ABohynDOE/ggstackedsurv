#' Tidy the state occupancy probabilities of a multi-state survfit object
#'
#' Internal helper shared by [ggstackedsurv()] (to draw the areas) and by the
#' risk table code. Returns one row per time and state.
#'
#' @param x A multi-state `survfit` object.
#' @param base_state A single string giving the name of the initial state.
#'
#' @return A data frame with columns `time`, `prob` and `state`. `state` is a
#'   factor whose levels give the stacking order used in the plot.
#' @noRd
.tidy_msurv <- function(x, base_state = "Censored") {
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

  data.frame(
    time = rep(time, times = ncol(pstate)),
    prob = as.vector(pstate),
    state = factor(
      rep(colnames(pstate), each = length(time)),
      levels = state_levels
    )
  )
}
