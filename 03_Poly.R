getwd()
data <- read.csv("data1.csv")
# STEP 5: Polynomial regression model
# Adding quadratic terms

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
     pch = 16, col = rgb(0,0,1,0.5))

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
     pch = 16, col = rgb(1,0,0,0.5))

# 4. Add horizontal line at 0
abline(h = 0, col = "blue", lwd = 2)
# STEP 7: Interpretation and notes

#Examples from Project 
#1 Linear model, OLS and Residuals
data_xy <- data[, c("mms_ammonia_kg", "tot_kg")]
data_xy <- na.omit(data_xy)
data_nonzero <- data_xy[data_xy$mms_ammonia_kg > 0, ]
subset_10 <- data_nonzero[1:10, ]
subset_10

write.csv(subset_10, "example_pairs1.csv", row.names = FALSE)
model <- lm(tot_kg ~ mms_ammonia_kg, data = subset_10)
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

plot(subset_10$mms_ammonia_kg / 1e9, subset_10$tot_kg / 1e6,
     xlab = "Ammonia demand (billions)",
     ylab = "Total kg (millions)",
     main = "OLS With Residual Lines (scaled axes)")

abline(lm(I(tot_kg / 1e6) ~ I(mms_ammonia_kg / 1e9), data = subset_10),
       col = "blue", lwd = 2)

segments(subset_10$mms_ammonia_kg / 1e9,
         subset_10$tot_kg / 1e6,
         subset_10$mms_ammonia_kg / 1e9,
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

# Cross-Validation

# Randomly shuffle data
df.shuffled <- model_data_log[sample(nrow(model_data_log)), ]

# Define number of folds to use for k-fold cross-validation
K <- 5

# Define max degree of polynomials to fit
degree <- 5

# Create k equal-sized folds
folds <- cut(seq(1, nrow(df.shuffled)), breaks = K, labels = FALSE)

# Create object to hold MSE's of models
mse <- matrix(data = NA, nrow = K, ncol = degree)

# Define predictor variables
predictors <- c("log_ammonia", "log_refineries", "log_metals",
                "log_ld_fcev", "log_mhd_fcev", "log_seasonal", "log_area")

# Count unique values per predictor (max usable degree = unique - 1)
max_degrees <- sapply(predictors, function(p) length(unique(df.shuffled[[p]])) - 1)

# Perform K-fold cross validation
for (i in 1:K) {
  
  # Define training and testing data
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  # Use k-fold cv to evaluate models with polynomial degrees 1 through 5
  for (j in 1:degree) {
    # Cap each predictor's degree at its max usable degree
    poly_terms <- paste0(
      "poly(", predictors, ", ", pmin(j, max_degrees), ")",
      collapse = " + "
    )
    formula_j <- as.formula(paste("log_tot_kg ~", poly_terms))
    
    fit.train <- lm(formula_j, data = trainData)
    fit.test <- predict(fit.train, newdata = testData)
    mse[i, j] <- mean((fit.test - testData$log_tot_kg)^2)
  }
}

# Find MSE for each degree
colMeans(mse)
