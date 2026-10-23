test_that("add_risktable() stores its settings without drawing anything", {
  rt <- add_risktable(times = c(0, 30), risktable_stats = "n.risk")
  expect_s3_class(rt, "ggstackedsurv_add_risktable")
  settings <- attr(rt, "ggstackedsurv_risktable")
  expect_equal(settings$times, c(0, 30))
  expect_equal(settings$risktable_stats, "{n.risk}")
})

test_that("add_risktable() validates its arguments", {
  expect_error(add_risktable(times = "a"), "times")
  expect_error(add_risktable(risktable_height = 2), "risktable_height")
  expect_error(add_risktable(risktable_stats = 1), "risktable_stats")
})

test_that("add_risktable() only works on ggstackedsurv plots", {
  p <- ggplot2::ggplot()
  expect_error(p + add_risktable(), "ggstackedsurv")
})

test_that("adding a risk table keeps the plot class and records the settings", {
  skip_if_not_installed("survival")
  p <- ggstackedsurv(ecmo_fit())
  p_rt <- p + add_risktable(times = c(0, 50))

  expect_s3_class(p_rt, "ggstackedsurv")
  expect_false(is.null(attr(p_rt, "ggstackedsurv")$survfit))
  expect_equal(length(p_rt$layers), length(p$layers) + 1)
  expect_false(is.null(.get_risktable_settings(p_rt)))
  expect_null(.get_risktable_settings(p))
})

test_that("risk table statistics match the survfit object", {
  skip_if_not_installed("survival")
  fit <- ecmo_fit()
  times <- c(0, 10, 20, 50, 200)
  stats <- .risktable_stats(fit, times)

  expect_equal(stats$time, times)
  expect_equal(stats$n.risk[1], fit$n)
  expect_equal(
    stats$n.risk,
    as.numeric(summary(fit, times = times, extend = TRUE)$n.risk[, 1])
  )
  # Everyone has left the initial state (event or censored) by day 200.
  expect_equal(stats$n.risk[length(times)], 0)
  expect_equal(
    stats$cum.event[length(times)] + stats$cum.censor[length(times)],
    fit$n
  )
  # Cumulative counts are the running sum of the interval counts.
  expect_equal(stats$cum.event, cumsum(stats$n.event))
  expect_equal(stats$cum.censor, cumsum(stats$n.censor))
})

test_that("cumulative counts are right when the table does not start at 0", {
  skip_if_not_installed("survival")
  fit <- ecmo_fit()
  from_zero <- .risktable_stats(fit, c(0, 20, 50))
  late <- .risktable_stats(fit, c(20, 50))
  expect_equal(late$cum.event, from_zero$cum.event[2:3])
})

test_that("glue syntax and custom labels are used for the rows", {
  skip_if_not_installed("survival")
  fit <- ecmo_fit()
  settings <- attr(
    add_risktable(
      risktable_stats = c("{n.risk} ({cum.event})", "n.censor"),
      stats_label = c("{n.risk} ({cum.event})" = "At risk (events)")
    ),
    "ggstackedsurv_risktable"
  )
  stats <- .risktable_stats(fit, c(0, 20))
  data <- .risktable_data(stats, settings)

  expect_equal(
    sort(levels(data$label)),
    sort(c("At risk (events)", "Censored"))
  )
  top <- data[data$label == "At risk (events)" & data$time == 0, "text"]
  expect_equal(top, paste0(fit$n, " (0)"))
})

test_that("ggstackedsurv_build() returns the plot and the table separately", {
  skip_if_not_installed("survival")
  p <- ggstackedsurv(ecmo_fit()) + add_risktable()
  parts <- ggstackedsurv_build(p, combine_plots = FALSE)

  expect_named(parts, c("plot", "table"))
  expect_s3_class(parts$plot, "ggplot")
  expect_s3_class(parts$table, "ggplot")
  expect_false(inherits(parts$plot, "ggstackedsurv"))
})

test_that("without a risk table the build returns the plain plot", {
  skip_if_not_installed("survival")
  p <- ggstackedsurv(ecmo_fit())
  expect_s3_class(ggstackedsurv_build(p), "ggplot")
  expect_null(ggstackedsurv_build(p, combine_plots = FALSE)$table)
})

test_that("the table uses the breaks of the final plot by default", {
  skip_if_not_installed("survival")
  p <- ggstackedsurv(ecmo_fit()) +
    ggplot2::scale_x_continuous(breaks = c(0, 45, 90)) +
    add_risktable()
  parts <- ggstackedsurv_build(p, combine_plots = FALSE)
  expect_setequal(unique(parts$table$data$time), c(0, 45, 90))
})

test_that("printing and saving draw the combined figure", {
  skip_if_not_installed("survival")
  p <- ggstackedsurv(ecmo_fit()) + add_risktable()
  expect_s3_class(ggstackedsurv_build(p), "patchwork")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))

  path <- withr::local_tempfile(fileext = ".png")
  expect_no_error(
    suppressMessages(ggplot2::ggsave(path, p, width = 6, height = 5))
  )
  expect_true(file.exists(path))
})
