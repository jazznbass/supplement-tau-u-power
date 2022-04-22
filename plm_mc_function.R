
plm_mc <- function(data) {

  data <- unlist(data, recursive = FALSE)

  y  <- data$values
  n1 <- sum(data$phase == "A")
  n2 <- sum(data$phase == "B")
  D <- c(rep(0, n1), rep(1, n2))
  mt <- data$mt
  inter <- (mt - mt[n1]) * D
  x <- matrix(
    c(rep(1, n1 + n2), mt, D, inter),
    nrow = n1 + n2,
    ncol = 4
  )

  full <- fastglm::fastglm(x = x, y = y, family = "poisson", method = 2)

  summary(full)$coef[3, 4]

}


