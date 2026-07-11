knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7.5,
  fig.path = "vigfig-"
)

run_expensive_vignette <- identical(
  tolower(Sys.getenv("LKT_RUN_EXPENSIVE_VIGNETTES")), "true")
knitr::opts_chunk$set(eval = run_expensive_vignette)
if (!run_expensive_vignette) {
  message(
    "Executable examples are disabled for routine package builds. Set ",
    "LKT_RUN_EXPENSIVE_VIGNETTES=true to run this vignette."
  )
}

library(LKT)
library(ggplot2)

prepare_largeraw_sample <- function(seed = 41) {
  set.seed(seed)
  val <- largerawsample

  val$KC..Default. <- val$Problem.Name
  val <- data.table::setDT(val)

  val$fold <- sample(1:5, length(val$Anon.Student.Id), replace = TRUE)

  unq <- sample(unique(val$Anon.Student.Id))
  sfold <- rep(1:5, length.out = length(unq))
  val$fold <- rep(0, length(val[, 1]))
  for (i in 1:5) {
    val$fold[which(val$Anon.Student.Id %in% unq[which(sfold == i)])] <- i
  }

  val$CF..Time. <- as.numeric(as.POSIXct(
    as.character(val$Time),
    format = "%Y-%m-%d %H:%M:%S"
  ))

  val <- val[order(val$Anon.Student.Id, val$CF..Time.), ]

  val$CF..ansbin. <- ifelse(
    tolower(val$Outcome) == "correct",
    1,
    ifelse(tolower(val$Outcome) == "incorrect", 0, -1)
  )
  val <- val[val$CF..ansbin. == 0 | val$CF..ansbin. == 1, ]

  val$Duration..sec. <- (
    val$CF..End.Latency. + val$CF..Review.Latency. + 500
  ) / 1000

  val <- computeSpacingPredictors(val, "KC..Default.")
  val <- computeSpacingPredictors(val, "KC..Cluster.")
  val <- computeSpacingPredictors(val, "Anon.Student.Id")
  val <- computeSpacingPredictors(val, "CF..Correct.Answer.")

  val
}

check_close <- function(label, actual, expected, tolerance = 1e-4) {
  if (!isTRUE(is.finite(actual)) ||
      abs(as.numeric(actual) - as.numeric(expected)) > tolerance) {
    stop(
      sprintf(
        "%s expected %s +/- %s but got %s",
        label,
        expected,
        tolerance,
        actual
      ),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: %s = %.6f\n", label, as.numeric(actual)))
  invisible(TRUE)
}

check_true <- function(label, condition) {
  if (!isTRUE(condition)) {
    stop(sprintf("%s check failed", label), call. = FALSE)
  }
  cat(sprintf("CHECK OK: %s\n", label))
  invisible(TRUE)
}

check_has_coefficients <- function(model, expected_names) {
  missing <- setdiff(expected_names, rownames(model$coefs))
  if (length(missing) > 0) {
    stop(
      sprintf("missing expected coefficients: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  cat(sprintf(
    "CHECK OK: expected coefficients present: %s\n",
    paste(expected_names, collapse = ", ")
  ))
  invisible(TRUE)
}

check_has_coefficient_matching <- function(model, pattern) {
  hits <- grep(pattern, rownames(model$coefs), value = TRUE)
  if (length(hits) == 0) {
    stop(
      sprintf("no coefficient matched pattern: %s", pattern),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: coefficient pattern matched: %s\n", pattern))
  invisible(TRUE)
}

check_has_coefficient_containing <- function(model, text) {
  hits <- grep(text, rownames(model$coefs), value = TRUE, fixed = TRUE)
  if (length(hits) == 0) {
    stop(
      sprintf("no coefficient contained text: %s", text),
      call. = FALSE
    )
  }
  cat(sprintf("CHECK OK: coefficient text found: %s\n", text))
  invisible(TRUE)
}

check_lkt_fit <- function(model, expected_r2, expected_loglike,
                          r2_tolerance = 1e-4,
                          loglike_tolerance = 1e-2,
                          min_prediction_n = 1) {
  check_true("model has coefficients", !is.null(model$coefs) && nrow(model$coefs) > 0)
  check_close("McFadden R2", model$r2, expected_r2, r2_tolerance)
  check_close("log likelihood", model$loglike, expected_loglike, loglike_tolerance)
  check_true(
    "prediction length",
    length(model$prediction) >= min_prediction_n || is.null(model$prediction)
  )
  invisible(TRUE)
}
