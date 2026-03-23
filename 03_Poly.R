getwd()
data <- read.csv("data1.csv")
# STEP 5: Polynomial regression model
# Adding squared terms to allow nonlinear scaling of sectoral hydrogen demand

model_poly <- lm(
  log_tot_kg ~ 
    log_ammonia + I(log_ammonia^2) +
    log_refineries + I(log_refineries^2) +
    log_metals + I(log_metals^2) +
    log_ld_fcev + I(log_ld_fcev^2) +
    log_mhd_fcev + I(log_mhd_fcev^2) +
    log_seasonal + I(log_seasonal^2) +
    log_area,
  data = model_data_log
)

summary(model_poly)
plot(model_poly$residuals)
hist(model_poly$residuals, breaks=50)
summary(model_poly)$adj.r.squared
AIC(model_linear_log, model_poly)

model_poly_cubic <- lm(
  log_tot_kg ~ 
    log_ammonia + I(log_ammonia^2) + I(log_ammonia^3) +
    log_refineries + I(log_refineries^2) + I(log_refineries^3) +
    log_metals + I(log_metals^2) + I(log_metals^3) +
    log_ld_fcev + I(log_ld_fcev^2) + I(log_ld_fcev^3) +
    log_mhd_fcev + I(log_mhd_fcev^2) + I(log_mhd_fcev^3) +
    log_seasonal + I(log_seasonal^2) + I(log_seasonal^3) +
    log_area,
  data = model_data_log
)

summary(model_poly_cubic)
AIC(model_linear_log, model_poly, model_poly_cubic)
# STEP 6: Model comparison (prediction performance)
# STEP 6.1: FITTED VS OBSERVED PLOT
# 1. Compute fitted values
fitted_values <- model_poly$fitted.values

# 2. Actual observed values
observed_values <- model_data_log$log_tot_kg

# 3. Basic scatter plot
plot(observed_values, fitted_values,
     xlab = "Observed log(total hydrogen demand)",
     ylab = "Fitted log(total hydrogen demand)",
     main = "Fitted vs Observed Values - Log-Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5)) # semi-transparent points

# 4. Add 45-degree reference line
abline(a = 0, b = 1, col = "red", lwd = 2)

#STEP 6.2 RESIDUALS VS PREDICTORS
# 1. Compute residuals from the log-polynomial model
residuals_poly <- model_poly$residuals

# 2. Predictor to check
predictor <- model_data_log$log_ld_fcev

# 3. Plot residuals vs predictor
plot(predictor, residuals_poly,
     xlab = "log(Light-duty FCEV hydrogen demand)",
     ylab = "Residuals",
     main = "Residuals vs log_ld_fcev - Log-Polynomial Model",
     pch = 16, col = rgb(1,0,0,0.5)) # semi-transparent red points

# 4. Add horizontal line at 0
abline(h = 0, col = "blue", lwd = 2)
# STEP 7: Interpretation and notes

#Examples from Project 
#1 Linear model, OLS and Residuals
data_xy <- data[, c("solar_kg", "tot_kg")]
data_xy <- na.omit(data_xy)
subset_10 <- data_xy[1:10, ]
subset_10

write.csv(subset_10, "example_pairs1.csv", row.names = FALSE)
model <- lm(tot_kg ~ solar_kg, data = subset_10)
summary(model)

subset_10$fitted <- fitted(model)
subset_10$residuals <- residuals(model)

subset_10

plot(subset_10$fitted, subset_10$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0)

plot(subset_10$fitted / 1e6,
     subset_10$residuals / 1e6,
     xlab = "Fitted values (millions)",
     ylab = "Residuals (millions)",
     main = "Residuals vs Fitted (scaled)")
abline(h = 0)

plot(subset_10$solar_kg / 1e9, subset_10$tot_kg / 1e6,
     xlab = "Solar energy (billions)",
     ylab = "Total kg (millions)",
     main = "OLS With Residual Lines (scaled axes)")

abline(lm(I(tot_kg / 1e6) ~ I(solar_kg / 1e9), data = subset_10),
       col = "blue", lwd = 2)

segments(subset_10$solar_kg / 1e9,
         subset_10$tot_kg / 1e6,
         subset_10$solar_kg / 1e9,
         subset_10$fitted / 1e6,
         col = "red")

# Fitted vs Observed
png("fitted_vs_observed_poly.png", width=800, height=600)
plot(observed_values, fitted_values,
     xlab = "Observed log(total hydrogen demand)",
     ylab = "Fitted log(total hydrogen demand)",
     main = "Fitted vs Observed - Log-Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5))
abline(a = 0, b = 1, col = "red", lwd = 2)
dev.off()

# Residuals plot
png("residuals_poly.png", width=800, height=600)
plot(fitted(model_poly), residuals(model_poly),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Polynomial Model",
     pch = 16, col = rgb(0,0,1,0.5))
abline(h = 0, col = "red", lwd = 2)
dev.off()

# ── LOESS SMOOTHS ─────────────────────────────────────────────
# Purpose: Visualise the functional form of each predictor relationship
# The varying shapes revealed by LOESS (U-shapes, convex, linear)
# motivate fractional polynomial regression in Chapter 6,
# where each predictor's transformation is selected from the data
# rather than fixed at a uniform degree.

predictors_log <- c("log_ammonia", "log_refineries", "log_metals",
                    "log_ld_fcev", "log_mhd_fcev", "log_seasonal",
                    "log_area")

for (var in predictors_log) {
  p <- ggplot(model_data_log, aes_string(x = var, y = "log_tot_kg")) +
    geom_point(alpha = 0.2, color = "steelblue", size = 0.8) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste("log_tot_kg vs", var),
         x = var, y = "log(Total Hydrogen Demand)")
  ggsave(paste0("scatter_loess_", var, ".png"), p, width = 8, height = 5)
}