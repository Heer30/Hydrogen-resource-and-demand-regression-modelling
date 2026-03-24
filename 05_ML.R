getwd()
data <- read.csv("data1.csv")
#Machine Learning: tree based method
names(data)
head(data[, c("name", "state_name", 
              "total_resource_minus_total_demand_kg")])
install.packages("rpart")
install.packages("randomForest")
library(rpart)
library(randomForest)

# Set up features and target
features <- c("mms_refineries_kg", "mms_metals_kg", "mms_ammonia_kg",
              "mms_biofuels_kg", "mms_synfuels_kg", "mms_ngas_kg",
              "mms_ld_fcev_kg", "mms_mhd_fcev_kg", 
              "mms_season_enrgy_strg_kg", "solar_kg", "wind_kg",
              "a_sqkm")

target <- "total_resource_minus_total_demand_kg"
# Prepare data
ml_data <- data[, c(features, target, "name", "state_name")]

# Check issues
cat("Rows:", nrow(ml_data), "\n")
cat("Missing values:", sum(is.na(ml_data)), "\n")

# Step 1: Decision Tree
set.seed(42)

tree_model <- rpart(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = ml_data,
  method = "anova",
  control = rpart.control(maxdepth = 4, minsplit = 20)
)

# Print summary of tree
printcp(tree_model)

# Plot the tree
png("decision_tree.png", width=1200, height=800)
plot(tree_model, uniform=TRUE, 
     main="Decision Tree - Hydrogen Production Suitability")
text(tree_model, use.n=TRUE, cex=0.7)
dev.off()

cat("Tree saved!\n")

# Step 2: Random Forest
set.seed(42)

rf_model <- randomForest(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = ml_data,
  ntree = 200,
  importance = TRUE
)

# Print summary
print(rf_model)

# Feature importance
importance(rf_model)

# Save feature importance plot
png("feature_importance.png", width=800, height=600)
varImpPlot(rf_model, 
           main="Feature Importance - Random Forest",
           type=1)
dev.off()

# Step 3: Rankings
# Add predictions back to data
ml_data$predicted_surplus <- predict(rf_model, ml_data)

# Top 20 counties
top20_counties <- head(ml_data[order(-ml_data$predicted_surplus), 
                               c("name", "state_name", 
                                 "predicted_surplus")], 20)
print(top20_counties)

# State level rankings: average predicted surplus per state
state_rankings <- aggregate(predicted_surplus ~ state_name, 
                            data = ml_data, FUN = mean)
state_rankings <- state_rankings[order(-state_rankings$predicted_surplus), ]
print(head(state_rankings, 20))

# Top 20 counties plot
png("top20_counties.png", width=900, height=700)
par(mar=c(5,12,4,2))
barplot(rev(top20_counties$predicted_surplus/1e9),
        names.arg=rev(paste(top20_counties$name, 
                            top20_counties$state_name, sep=", ")),
        horiz=TRUE, las=1, cex.names=0.7,
        xlab="Predicted Surplus (billion kg)",
        main="Top 20 Counties by Hydrogen Production Suitability",
        col="#2E86AB")
dev.off()

# Top 20 states plot
top20_states <- head(state_rankings, 20)
png("top20_states.png", width=900, height=700)
par(mar=c(5,10,4,2))
barplot(rev(top20_states$predicted_surplus/1e9),
        names.arg=rev(top20_states$state_name),
        horiz=TRUE, las=1, cex.names=0.8,
        xlab="Mean Predicted Surplus (billion kg)",
        main="Top 20 States by Hydrogen Production Suitability",
        col="#A23B72")
dev.off()

cat("Plots saved!\n")

# Get R squared for random forest
rf_r2 <- 1 - sum((ml_data$total_resource_minus_total_demand_kg - 
                    ml_data$predicted_surplus)^2) /
  sum((ml_data$total_resource_minus_total_demand_kg - 
         mean(ml_data$total_resource_minus_total_demand_kg))^2)

cat("Random Forest R squared:", round(rf_r2, 4), "\n")
cat("Polynomial R squared: 0.7656\n")
cat("Log-Linear R squared: 0.7083\n")

# Train test split 80/20
set.seed(42)
train_idx <- sample(1:nrow(ml_data), 0.8 * nrow(ml_data))
train_data <- ml_data[train_idx, ]
test_data  <- ml_data[-train_idx, ]

# Retrain on training data
rf_model_test <- randomForest(
  total_resource_minus_total_demand_kg ~ mms_refineries_kg + 
    mms_metals_kg + mms_ammonia_kg + mms_biofuels_kg + 
    mms_synfuels_kg + mms_ngas_kg + mms_ld_fcev_kg + 
    mms_mhd_fcev_kg + mms_season_enrgy_strg_kg + 
    solar_kg + wind_kg + a_sqkm,
  data = train_data,
  ntree = 200,
  importance = TRUE
)

# Test R squared
test_preds <- predict(rf_model_test, test_data)
test_r2 <- 1 - sum((test_data$total_resource_minus_total_demand_kg - 
                      test_preds)^2) /
  sum((test_data$total_resource_minus_total_demand_kg - 
         mean(test_data$total_resource_minus_total_demand_kg))^2)

cat("Test R squared:", round(test_r2, 4), "\n")
