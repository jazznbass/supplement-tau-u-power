## p ---------
tau_u_trendA <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "p")]
}

tau_u_trendA_trendB <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "p")]
}

tau_u_AB <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B"), which(names(res) == "p")]
}

tau_u_base <- function(x) {
  corrected_tau(x, continuity = FALSE, repeated = FALSE)$p
}



# ES ------------------------------

tau_u_trendA_es <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "Tau")]
}

tau_u_trendA_trendB_es <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "Tau")]
}

tau_u_AB_es <- function(x) {
  res <- tau_u(x, method = "complete", tau_method = "a", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B"), which(names(res) == "Tau")]
}

tau_u_base_es <- function(x) {
  corrected_tau(x, continuity = FALSE, repeated = FALSE)$tau
}

## aggregate functions ----------------------

mc_perc_sig <- function(x) {
  mean(x <= 0.05, na.rm = TRUE) * 100
}

mc_mean <- function(x) {
  mean(x, na.rm = TRUE)
}

mc_quantile <- function(x) {
  out <- list()
  x <- quantile(x, c(0.16, 0.5, 0.84))
  out$P16 <- x[1]
  out$P50 <- x[2]
  out$P84 <- x[3]
  out
}

mc_quantile2 <- function(x) {
  out <- list()
  x <- quantile(x)
  out$P25 <- x[2]
  out$P50 <- x[3]
  out$P75 <- x[4]
  out
}
