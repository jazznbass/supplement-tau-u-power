design_template <- function(...) {
  out <- as.list(substitute(...()))
  valid <- names(formals(scan::design))
  check <- sapply(names(out), function(x) !x %in% valid)
  if (any(check)) {
    stop("\nWrong argument(s): ",
         paste(names(out)[check], collapse = ", "))
  }
  out
}

.sec_time <- function(sec) {
  sec <- round(sec)
  h <- sec %/% 3600
  minutes <- (sec - h * 3600) %/% 60
  sec <- sec - (minutes * 60 + h * 3600)
  sprintf("%d:%02d:%02d", h, minutes, sec)
}

.progress_feedback <- function(counter, total, seconds) {
  if (counter == 0) {
    perc_done <- 0
    sec_remain <- NA
  } else {
    perc_done <- counter / total * 100
    sec_remain <- (100 - perc_done) * (seconds / perc_done)
  }
  cat(
    "\rProgress ",
    sprintf("%3d", round(perc_done)), "%; ",
    .sec_time(seconds), "; remain ",
    .sec_time(sec_remain),
    sep = ""
  )
}

mc_scan <- function(design,
                    iterations,
                    method,
                    alpha_test = TRUE,
                    power_test = TRUE,
                    store_design = FALSE) {

  starttime <- proc.time()

  n_iterations <- prod(sapply(iterations, length))
  counter <- 0

  return_pa <- list()

  iter <- expand.grid(iterations)

  mc_env <- new.env()

  for(i in 1:nrow(iter)) {

    x <- paste0(names(iter[i,]), " = ", iter[i,])
    x <- lapply(x, str2lang)
    for(j in x) eval(j, envir = mc_env)

    new_design <- do.call(
      "design",
      lapply(design, function(x) eval(x, envir = mc_env))
    )

    pa <- power_test(
      design = new_design,
      method = method,
      design_is_one_study = FALSE,
      alpha_test = alpha_test,
      power_test = power_test,
      binom_test = FALSE
    )

    attr(pa, "iter") <- iter[i,]

    if (store_design) attr(pa, "design") <- new_design

    return_pa <- c(return_pa, list(pa))

    # output remaining time
    counter <- counter + 1
    .progress_feedback(counter, n_iterations, (proc.time() - starttime)[3])
  }

  cat("\n")

  attr(return_pa, "iterations") <- iterations
  attr(return_pa, "design") <- design
  attr(return_pa, "computation_duration") <- proc.time() - starttime

  return_pa
}
