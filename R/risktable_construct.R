#' Statistics shown in the risk table
#'
#' Computes, at each requested time, the statistics for the subjects in the
#' initial state of a multi-state `survfit` object.
#'
#' @param fit A multi-state `survfit` object.
#' @param times Numeric vector of times.
#'
#' @return A data frame with one row per time and columns `time`, `n.risk`,
#'   `n.event`, `cum.event`, `n.censor` and `cum.censor`.
#' @noRd
.risktable_stats <- function(fit, times) {
  times <- sort(unique(times))

  # Number at risk: subjects currently in the initial state (first column).
  s <- summary(fit, times = times, extend = TRUE)
  n_risk <- as.numeric(s$n.risk[, 1])

  # Cumulative counts are read from the unsummarised fit, so they are right
  # even when the first time shown is not 0.
  cumulate <- function(per_time) {
    cum <- c(0, cumsum(per_time))
    cum[findInterval(times, fit$time) + 1]
  }
  # Transitions out of the initial state = events entering any other state.
  events <- rowSums(fit$n.event[, -1, drop = FALSE])
  cum_event <- cumulate(events)
  cum_censor <- cumulate(fit$n.censor)

  data.frame(
    time = times,
    n.risk = n_risk,
    n.event = diff(c(0, cum_event)),
    cum.event = cum_event,
    n.censor = diff(c(0, cum_censor)),
    cum.censor = cum_censor
  )
}

# Default row labels for the built-in statistics.
.risktable_default_labels <- c(
  n.risk = "At risk",
  n.event = "Events",
  cum.event = "Cum. events",
  n.censor = "Censored",
  cum.censor = "Cum. censored"
)

#' Long data for the risk table plot
#'
#' @param stats Output of `.risktable_stats()`.
#' @param settings The settings stored by [add_risktable()].
#'
#' @return A data frame with columns `time`, `label` (factor, first row last
#'   so it is drawn at the top) and `text`.
#' @noRd
.risktable_data <- function(stats, settings) {
  raw <- settings$risktable_stats_raw
  templates <- settings$risktable_stats

  labels <- .risktable_labels(raw, settings$stats_label)

  rows <- lapply(seq_along(templates), function(i) {
    data.frame(
      time = stats$time,
      label = labels[[i]],
      text = as.character(glue::glue_data(stats, templates[[i]], .na = "NA")),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # First statistic at the top of the table.
  out$label <- factor(out$label, levels = rev(unique(labels)))
  out
}

.risktable_labels <- function(raw, stats_label) {
  labels <- vapply(
    raw,
    function(s) {
      if (s %in% names(.risktable_default_labels)) {
        .risktable_default_labels[[s]]
      } else {
        s
      }
    },
    character(1)
  )
  if (!is.null(stats_label)) {
    stats_label <- unlist(stats_label)
    if (is.null(names(stats_label))) {
      if (length(stats_label) != length(raw)) {
        cli::cli_abort(
          "An unnamed {.arg stats_label} must have one element per element of {.arg risktable_stats}."
        )
      }
      labels[] <- stats_label
    } else {
      hit <- intersect(names(stats_label), raw)
      labels[hit] <- stats_label[hit]
    }
  }
  unname(labels)
}

#' Draw the risk table
#'
#' @param data Output of `.risktable_data()`.
#' @param xlim Numeric length 2: x range of the main plot panel.
#' @param settings The settings stored by [add_risktable()].
#'
#' @return A ggplot object.
#' @noRd
.construct_risktable <- function(data, xlim, settings) {
  text_layer <- do.call(ggplot2::geom_text, settings$text_args)

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$time, y = .data$label, label = .data$text)
  ) +
    text_layer +
    # Same x range and no expansion as the main plot, so numbers sit under the
    # ticks. Clipping is off so the label at the panel edge is not cut in half.
    # The y limits give every row an equal band (rows sit at 1, 2, ...), since
    # `expand = FALSE` would otherwise place the first and last rows exactly on
    # the panel edges.
    ggplot2::coord_cartesian(
      xlim = xlim,
      ylim = c(0.5, nlevels(data$label) + 0.5),
      expand = FALSE,
      clip = "off"
    ) +
    settings$theme
}
