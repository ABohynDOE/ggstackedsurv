ecmo <- read.csv("data-raw/ecmo.csv")
ecmo$status <- factor(
  ecmo$status,
  levels = c(
    "censored",
    "Died",
    "Alive and out of ICU",
    "In ICU and weaned off ECMO"
  )
)
ecmo$id <- as.factor(ecmo$id)
attr(ecmo$id, "label") <- "Patient ID"
attr(ecmo$time, "time") <- "Days from ECMO onset"
attr(ecmo$status, "label") <- "Status"
usethis::use_data(ecmo, overwrite = TRUE)
