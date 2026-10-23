ecmo_fit <- function() {
  survival::survfit(survival::Surv(time, status) ~ 1, data = ecmo)
}
