
tau_u_trendA <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "p")]
}

tau_u_trendA_trendB <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "p")]
}

tau_u_AB <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B"), which(names(res) == "p")]
}

tau_u_base <- function(x) {
  corrected_tau(x, continuity = FALSE, repeated = FALSE)$p
}



# ES ------------------------------

tau_u_trendA_es <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B - Trend A"), which(names(res) == "Tau")]
}

tau_u_trendA_trendB_es <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B + Trend B - Trend A"), which(names(res) == "Tau")]
}

tau_u_AB_es <- function(x) {
  res <- tau_u(x, method = "parker", meta_method = "none")$table[[1]]
  res[which(row.names(res) == "A vs. B"), which(names(res) == "Tau")]
}

tau_u_base_es <- function(x) {
  corrected_tau(x, continuity = FALSE, repeated = FALSE)$tau
}
