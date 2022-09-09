
mc_power_test <- function(design,
                       method = c("plm_level", "rand", "tauU"), 
                       effect = "level",
                       n_sim = 100,
                       design_is_one_study = TRUE,
                       eval_function = "perc_sig",
                       alpha_beta = TRUE,
                       correct = TRUE,
                       alpha_test = TRUE,
                       power_test = TRUE,
                       binom_test = 0.5,
                       alpha_level = 0.05,
                       labels = c("Power", "Alpha Error")) {
  
  
  starttime <- proc.time()
  
  if (identical(eval_function, "perc_sig")) eval_function <- mc_perc_sig
  if (identical(eval_function, "mean")) eval_function <- mc_mean
  
  mc_fun <- unlist(
    lapply(
      method, 
      function(x) if (inherits(x, "character")) scan:::mc_function(x) else x
    ),
    recursive = FALSE
  )
  
  # return object
  out <- data.frame(Method = names(mc_fun))
  
  # power calculation ----------
  
  if (power_test) {
    mc_tab <- .mc_scdf(
      design = design, n_sim = n_sim, 
      alpha_level = alpha_level, mc_fun = mc_fun, 
      design_is_one_study = design_is_one_study,
      eval_function = eval_function
    )
    if (nrow(mc_tab) > 1) mc_tab <- t(mc_tab)
    out <- cbind(out, mc_tab)
  } else out$Power <- NA
  
  # alpha error calculation ----------
  
  if (alpha_test) {
    
    #level <- any(sapply(design$cases, function(x) x$level) != 0)
    #slope <- any(sapply(design$cases, function(x) x$slope) != 0)
    
    design_no_effect <- design
    if (effect == "level") {
      design_no_effect$cases <- lapply(
        design_no_effect$cases, 
        function(x) {x$level <- rep(0, length = length(x$length)); x}
      )
    }
    
    if (effect == "slope") {
      design_no_effect$cases <- lapply(
        design_no_effect$cases, 
        function(x) {x$slope <- rep(0, length = length(x$length)); x}
      )
    }
    
    mc_tab <- .mc_scdf(
      design = design_no_effect, n_sim = n_sim, 
      alpha_level = alpha_level, mc_fun = mc_fun, 
      design_is_one_study = design_is_one_study,
      eval_function = eval_function
    )
    if (nrow(mc_tab) > 1) mc_tab <- t(mc_tab)
    out <- cbind(out, mc_tab)
  } else out$"Alpha Error" <- NA
  
  if(!identical(labels, NA)) names(out)[2:(length(labels) + 1)] <- labels
  
  if (alpha_beta)
    out$"Alpha:Beta" <- sprintf("1:%.1f", (100 - out$Power) / out$"Alpha Error")
  
  if (correct)
    out$Correct <- (out$Power + (100 - out$"Alpha Error")) / 2
  
  if (!isFALSE(binom_test) && !isFALSE(correct)) {
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim * 2), n_sim * 2, p = binom_test)
      round(x$p.value, 3)
    }
    out$p <- sapply(out$Correct, b_test)
    
  }
  
  attr(out, "computation_duration") <- proc.time() - starttime
  class(out) <- c("sc_power")
  out
}

.mc_scdf <- function(design, 
                     alpha_level = NA, 
                     n_sim, 
                     mc_fun, 
                     design_is_one_study,
                     eval_function) {
  
  # Genrate random sample ----------------------------------------------------
  rand_sample <- list()
  
  if (design_is_one_study) {
    for(i in 1:n_sim) rand_sample[[i]] <- random_scdf(design = design)
  }
  
  if (!design_is_one_study) {
    tmp <- random_scdf(design = design)
    for (i in seq_along(tmp)) rand_sample[[i]] <- tmp[i]
  }
  
  # analyse random sample ---------------------------------------------------
  
  test_function <- function(func) {
    x <- sapply(rand_sample, func)
    do.call(eval_function, list(x))
  }
  out <-  sapply(mc_fun, test_function) 
  
  # return
  out
}
