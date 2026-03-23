getwd()
data <- read.csv("data1.csv")
#Step 1: Fractional Polynomial Model
library(mfp)
# Fractional Polynomial Model
fp_model <- mfp(
  log_tot_kg ~ 
    fp(log_ammonia) +
    fp(log_refineries) +
    fp(log_metals) +
    fp(log_ld_fcev) +
    fp(log_mhd_fcev) +
    fp(log_seasonal) +
    log_area,
  data = model_data_log,
  family = gaussian,
  verbose = TRUE
)
#Step 1: Fractional
# Model summary
summary(fp_model)

# AIC comparison with previous models
AIC(model_linear_log, model_poly, model_poly_cubic, fp_model)

# Fitted vs observed plot
plot(model_data_log$log_tot_kg, fp_model$fitted.values,
     xlab = "Observed log(tot_kg)",
     ylab = "Fitted log(tot_kg)",
     main = "FP Model: Fitted vs Observed",
     pch = 16, col = rgb(0, 0, 1, 0.4))
abline(0, 1, col = "red", lwd = 2)

# Residual plot
plot(fp_model$fitted.values, fp_model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "FP Model: Residuals vs Fitted",
     pch = 16, col = rgb(0, 0, 1, 0.4))
abline(h = 0, col = "red", lwd = 2)
summary(fp_model)
