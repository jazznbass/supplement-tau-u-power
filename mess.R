# mess around

design <- design(
  n = 1,
  start_value = 50,
  s = 10,
  level = 2,
  trend = 0,
  slope = 0,
  phase_design = list(A = 10, B = 20),
  distribution = "gaussian"
)

power_test(
  design,
  method = list(
    "Tau-U A vs. B" = tau_u_AB,
    "Tau-U trendA" = tau_u_trendA, 
    "Tau-U trendA + trendB" = tau_u_trendA_trendB, 
    "Tau-U adjusted" = tau_u_base
  )
)


plot_mc(out, caption = FALSE) 
out


# ----------------


# methods_p <- list(
#   "Tau-U A vs. B" = get_mcfn("tau_u_AB"),
#   "Tau-U trendA" = get_mcfn("tau_u_trendA"), 
#   "Tau-U trendA + trendB" = get_mcfn("tau_u_trendA_trendB"), 
#   "Tau-U adjusted" = get_mcfn("tau_u_base")
# )
# 
# methods_es <- list(
#   "Tau-U A vs. B" = get_mcfn("tau_u_AB_es"),
#   "Tau-U trendA" = get_mcfn("tau_u_trendA_es"), 
#   "Tau-U trendA + trendB" = get_mcfn("tau_u_trendA_trendB_es"), 
#   "Tau-U adjusted" = get_mcfn("tau_u_base_es")
# )
