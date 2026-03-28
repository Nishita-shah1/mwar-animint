# Moving Window Auto-Regression - animint2 port
# animation package original: https://yihui.org/animation/example/mwar-ani/

library(animint2)

set.seed(7)

n           <- 100
window_size <- 15
x           <- as.numeric(arima.sim(list(ar = 0.7), n = n))
windows     <- seq(window_size, n, by = 1)

time_df <- data.frame(t = seq_len(n), value = x)

window_df <- do.call(rbind, lapply(windows, function(w) {
  idx <- (w - window_size + 1):w
  data.frame(
    w        = w,
    t        = idx,
    value    = x[idx],
    point_id = seq_along(idx)
  )
}))

ar_list <- lapply(windows, function(w) {
  idx  <- (w - window_size + 1):w
  window_data <- x[idx]
  fit  <- ar(window_data, aic = FALSE, order.max = 1)
  
  phi <- as.numeric(fit$ar)
  if(length(phi) == 0) phi <- 0
  
  fitted_vals <- c(NA, phi * window_data[-window_size])
  resids <- window_data - fitted_vals
  
  list(
    coef_df = data.frame(w = w, ar_coef = phi, resid_sd = sd(resids, na.rm = TRUE)),
    resid_df = data.frame(w = w, t = idx, residual = resids, point_id = seq_along(idx)),
    label_df = data.frame(w = w, x_pos = 50, y_pos = 4, 
                          label = sprintf("Window: %d\nAR(1) Coef: %.2f", w, phi))
  )
})

ar_df    <- do.call(rbind, lapply(ar_list, `[[`, "coef_df"))
resid_df <- do.call(rbind, lapply(ar_list, `[[`, "resid_df"))
label_df <- do.call(rbind, lapply(ar_list, `[[`, "label_df"))

pts <- ggplot() +
  geom_line(data = time_df, aes(x = t, y = value), color = "gray60", size = 0.8) +
  geom_tallrect(
    data = ar_df,
    aes(xmin = w - window_size, xmax = w),
    showSelected = "w",
    fill = "steelblue", alpha = 0.15
  ) +
  geom_line(
    data = window_df,
    aes(x = t, y = value, key = point_id),
    showSelected = "w",
    color = "steelblue", size = 1.5
  ) +
  geom_text(
    data = label_df,
    aes(x = x_pos, y = y_pos, label = label),
    showSelected = "w",
    size = 10, fontface = "bold"
  ) +
  labs(title = "Time Series Moving Window", x = "Time", y = "Value") +
  theme_bw()

pcoef <- ggplot() +
  geom_tallrect(
    data = ar_df,
    aes(xmin = w - 0.5, xmax = w + 0.5),
    clickSelects = "w",
    alpha = 0.2, fill = "gold"
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.7, color = "gray50", linetype = "dotted") +
  geom_line(data = ar_df, aes(x = w, y = ar_coef), color = "gray40") +
  geom_point(
    data = ar_df,
    aes(x = w, y = ar_coef, key = w),
    showSelected = "w",
    color = "orange", size = 5
  ) +
  labs(title = "Estimated AR(1) Coefficient", x = "Window Center", y = "Coefficient") +
  theme_bw()

presid <- ggplot() +
  geom_tallrect(
    data = ar_df,
    aes(xmin = w - 0.5, xmax = w + 0.5),
    clickSelects = "w",
    alpha = 0.2, fill = "gold"
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point(
    data = resid_df,
    aes(x = t, y = residual, key = point_id),
    showSelected = "w",
    color = "steelblue", size = 2
  ) +
  labs(title = "AR(1) Residuals", x = "Time", y = "Residual") +
  theme_bw()

psd <- ggplot() +
  geom_tallrect(
    data = ar_df,
    aes(xmin = w - 0.5, xmax = w + 0.5),
    clickSelects = "w",
    alpha = 0.2, fill = "gold"
  ) +
  geom_line(data = ar_df, aes(x = w, y = resid_sd), color = "darkgreen") +
  geom_point(
    data = ar_df,
    aes(x = w, y = resid_sd, key = w),
    showSelected = "w",
    color = "orange", size = 5
  ) +
  labs(title = "Residual SD", x = "Window Center", y = "SD") +
  theme_bw()

viz <- animint(
  tsplot   = pts,
  coef     = pcoef,
  resid    = presid,
  sdplot   = psd,
  first    = list(w = windows[1]),
  duration = list(w = 250),
  time     = list(variable = "w", ms = 400),
  title    = "Moving Window AR(1) Analysis",
  source   = "https://github.com/Nishita-shah1/mwar-animint"
)

print(viz)
animint2pages(viz, "mwar-animint")
