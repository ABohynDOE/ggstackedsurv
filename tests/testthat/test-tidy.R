test_that(".tidy_msurv() starts every curve at time 0 in the base state", {
  skip_if_not_installed("survival")
  tidy <- .tidy_msurv(ecmo_fit(), base_state = "Censored")

  expect_named(tidy, c("time", "prob", "state"))
  expect_equal(min(tidy$time), 0)
  at_zero <- tidy[tidy$time == 0, ]
  expect_equal(at_zero$prob[at_zero$state == "Censored"], 1)
  expect_equal(sum(at_zero$prob), 1)
})

test_that(".tidy_msurv() probabilities sum to 1 at every time", {
  skip_if_not_installed("survival")
  tidy <- .tidy_msurv(ecmo_fit())
  totals <- as.numeric(tapply(tidy$prob, tidy$time, sum))
  expect_equal(totals, rep(1, length(totals)))
})

test_that("the event of interest is stacked last", {
  skip_if_not_installed("survival")
  fit <- ecmo_fit()
  tidy <- .tidy_msurv(fit)
  expect_equal(levels(tidy$state)[nlevels(tidy$state)], fit$states[2])
})
