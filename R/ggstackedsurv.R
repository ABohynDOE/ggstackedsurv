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
#' @examples
#' if (require("survival")) {
#'  # Fit a multi-state model with the "ecmo" dataset
#'  fit <- survfit(Surv(time, status) ~ 1, data = ecmo)
#'  # Generate a stacked probability plot
#'  ggstackedsurv(fit, base_state = "On ECMO")
#' }
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

  tidy <- .tidy_msurv(x, base_state = base_state)
  time <- unique(tidy$time)
  probs_long <- data.frame(x = tidy$time, y = tidy$prob, state = tidy$state)

  # Step each state separately so the curves hold until the next time.
  probs_step <- ggplot2:::dapply(probs_long, "state", ggplot2:::stairstep)

  p <- ggplot2::ggplot(probs_step, ggplot2::aes(x = x, y = y, fill = state)) +
    ggplot2::geom_area(color = "black", linewidth = 0.3) +
    ggplot2::labs(x = "time", y = "Probability") +
    ggplot2::coord_cartesian(xlim = range(time), expand = FALSE) +
    theme

  # Keep the fit so add_risktable() can compute table values from it.
  attr(p, "ggstackedsurv") <- list(
    survfit = x,
    states = c(base_state, x$states[-1]),
    base_state = base_state
  )
  class(p) <- c("ggstackedsurv", class(p))
  p
}
