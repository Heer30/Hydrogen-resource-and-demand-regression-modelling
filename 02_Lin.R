getwd()
data <- read.csv("data1.csv")
# STEP 4: Baseline linear regression model
# --- Baseline linear model (raw scale) ---
# Purpose: Provide interpretable benchmark
# Issue: Poor fit due to zero inflation and skewness

model_linear <- lm(
  tot_kg ~ mms_ammonia_kg +
    mms_refineries_kg +
    mms_metals_kg +
    mms_ld_fcev_kg +
    mms_mhd_fcev_kg +
    mms_season_enrgy_strg_kg +
    a_sqkm,
  data = model_data
)

summary(model_linear)

colMeans(model_data == 0) # check structural zeros
hist(model_data$mms_ammonia_kg, breaks = 50)

# Predicted values from the linear model
predicted <- fitted(model_linear)

# Observed values
observed <- model_data$tot_kg

# Scatter plot
plot(predicted, observed,
     xlab = "Predicted Total Hydrogen Demand",
     ylab = "Observed Total Hydrogen Demand",
     main = "Predicted vs Observed Hydrogen Demand",
     pch = 19, col = rgb(0,0,1,0.3)) # semi-transparent blue points

# Add a 45-degree reference line (perfect prediction)
abline(a = 0, b = 1, col = "red", lwd = 2)

png("Rplot1.png", width=800, height=600)
plot(predicted, observed,
     xlab = "Predicted Total Hydrogen Demand",
     ylab = "Observed Total Hydrogen Demand",
     main = "Predicted vs Observed Hydrogen Demand",
     pch = 19, col = rgb(0,0,1,0.3))
abline(a=0,b=1,col="red",lwd=2)
dev.off()


png("residuals_linear.png", width=800, height=600)
plot(fitted(model_linear), residuals(model_linear),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Linear Model",
     pch = 19, col = rgb(0,0,1,0.3))
abline(h = 0, col = "red", lwd = 2)
dev.off()
# STEP 4.2: Log Transformed Linear Regression Model
model_data_log <- transform(
  model_data,
  log_tot_kg = log(tot_kg),
  log_ammonia = log1p(mms_ammonia_kg),
  log_refineries = log1p(mms_refineries_kg),
  log_metals = log1p(mms_metals_kg),
  log_ld_fcev = log1p(mms_ld_fcev_kg),
  log_mhd_fcev = log1p(mms_mhd_fcev_kg),
  log_seasonal = log1p(mms_season_enrgy_strg_kg),
  log_area = log(a_sqkm)
)
summary(model_data_log)

#STEP 4.2 log-linear model
model_linear_log <- lm(
  log_tot_kg ~ log_ammonia +
    log_refineries +
    log_metals +
    log_ld_fcev +
    log_mhd_fcev +
    log_seasonal +
    log_area,
  data = model_data_log
)

summary(model_linear_log)

AIC(model_linear, model_linear_log)

