#01.EDA
getwd()
data <- read.csv("data1.csv")
str(data)
summary(data)
names(data)
# STEP 2: Inspect and understand the data
# Response

#tot_kg: total hydrogen serviceable consumption potential (county-level)

#Predictors

#Industrial: mms_ammonia_kg, mms_refineries_kg, mms_metals_kg
ammonia_subset <- model_data[model_data$mms_ammonia_kg > 0, 
                             c("mms_ammonia_kg", "tot_kg")]
ammonia_subset <- head(ammonia_subset, 10)
ammonia_subset
mean(ammonia_subset$mms_ammonia_kg)
mean(ammonia_subset$tot_kg)
x <- ammonia_subset$mms_ammonia_kg
y <- ammonia_subset$tot_kg

x_bar <- mean(x)
y_bar <- mean(y)

beta1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
beta1

beta0 <- y_bar - beta1 * x_bar
beta0
y_hat_example <- beta0 + beta1 * x
residuals_example <- y - y_hat_example

RSS <- sum((y - y_hat_example)^2)
TSS <- sum((y - mean(y))^2)
R2 <- 1 - (RSS/TSS)
adj_R2 <- 1 - ((1 - R2) * (10 - 1)) / (10 - 1 - 1)
sigma2 <- RSS / (10 - 1 - 1)

cat("RSS:", RSS, "\n")
cat("TSS:", TSS, "\n")
cat("R2:", R2, "\n")
cat("Adj R2:", adj_R2, "\n")
cat("Sigma squared:", sigma2, "\n")

data.frame(
  y = y,
  y_hat = round(y_hat_example),
  epsilon = round(residuals_example)
)
#Transport: mms_ld_fcev_kg, mms_mhd_fcev_kg

#Energy/storage: mms_season_enrgy_strg_kg



# STEP 3: Prepare variables for modeling

response <- "tot_kg" 

model_data <- data[,c( "tot_kg",
                       "mms_ammonia_kg",
                       "mms_refineries_kg",
                       "mms_metals_kg",
                       "mms_ld_fcev_kg",
                       "mms_mhd_fcev_kg",
                       "mms_season_enrgy_strg_kg",
                       "a_sqkm")]
summary(model_data)
#Control: a_sqkm
colSums(is.na(model_data))

mean(model_data$mms_ammonia_kg == 0)
library(ggplot2)

library(corrplot)
library(maps)

#1. DISTRIBUTION PLOTS
# Response variable
ggplot(model_data, aes(x = tot_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Total Hydrogen Demand",
       x = "Total Hydrogen Demand (kg)", y = "Frequency")
ggsave("hist_tot_kg.png", width = 8, height = 5)

# Log-transformed response
ggplot(model_data_log, aes(x = log_tot_kg)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of log(Total Hydrogen Demand)",
       x = "log(Total Hydrogen Demand)", y = "Frequency")
ggsave("hist_log_tot_kg.png", width = 8, height = 5)

# All predictors on log scale
predictors_log <- c("log_ammonia", "log_refineries", "log_metals",
                    "log_ld_fcev", "log_mhd_fcev", "log_seasonal",
                    "log_area")

for (var in predictors_log) {
  p <- ggplot(model_data_log, aes_string(x = var)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    labs(title = paste("Distribution of", var), 
         x = var, y = "Frequency")
  ggsave(paste0("hist_", var, ".png"), p, width = 8, height = 5)
}

# 2. ZERO INFLATION ANALYSIS 
zero_props <- colMeans(model_data == 0) * 100
zero_df <- data.frame(
  variable = names(zero_props),
  pct_zero = zero_props
)
zero_df <- zero_df[zero_df$pct_zero > 0, ]

ggplot(zero_df, aes(x = reorder(variable, pct_zero), y = pct_zero)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage of Zero Values by Variable",
       x = "Variable", y = "Percentage of Zeros (%)") +
  geom_text(aes(label = paste0(round(pct_zero, 1), "%")), 
            hjust = -0.1, size = 3.5)
ggsave("zero_inflation.png", width = 8, height = 5)

#3. CORRELATION ANALYSIS 
cor_matrix <- cor(model_data_log[, c("log_tot_kg",
                                     predictors_log)],
                  use = "complete.obs")

png("correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         title = "Correlation Matrix of Log-Transformed Variables",
         mar = c(0, 0, 2, 0))
dev.off()

# 4. SCATTER PLOTS OF KEY RELATIONSHIPS
# Plot each predictor against response
for (var in predictors_log) {
  p <- ggplot(model_data_log, aes_string(x = var, y = "log_tot_kg")) +
    geom_point(alpha = 0.2, color = "steelblue", size = 0.8) +
    labs(title = paste("log_tot_kg vs", var),
         x = var, y = "log(Total Hydrogen Demand)")
  ggsave(paste0("scatter_", var, ".png"), p, width = 8, height = 5)
}

# SCATTER PLOTS (key relationships only)
key_vars <- c("log_ld_fcev", "log_mhd_fcev", 
              "log_seasonal", "log_area")

for (var in key_vars) {
  p <- ggplot(model_data_log, aes_string(x = var, 
                                         y = "log_tot_kg")) +
    geom_point(alpha = 0.2, color = "steelblue", size = 0.8) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = paste("log_tot_kg vs", var),
         x = var, y = "log(Total Hydrogen Demand)")
  ggsave(paste0("scatter_", var, ".png"), p, width = 8, height = 5)
}
}

# 5. SPATIAL ANALYSIS 

county_map <- map_data("county")


spatial_data <- data.frame(
  fips     = data$fips,
  tot_kg   = data$tot_kg,
  state    = tolower(data$state_name),
  county   = tolower(data$name)
)

us_map <- map_data("state")

# State-level 
state_data <- aggregate(tot_kg ~ state, 
                        data = spatial_data, 
                        FUN = mean)

map_merged <- merge(us_map, state_data, 
                    by.x = "region", 
                    by.y = "state")
map_merged <- map_merged[order(map_merged$order), ]

ggplot(map_merged, aes(x = long, y = lat, 
                       group = group, fill = tot_kg / 1e6)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(low = "lightyellow", high = "darkred",
                      name = "Mean Demand\n(millions kg)") +
  coord_fixed(1.3) +
  labs(title = "Mean Total Hydrogen Demand by State",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("spatial_map.png", width = 10, height = 6)

#ZERO INFLATION BAR CHART
zero_props <- colMeans(model_data == 0) * 100
zero_df <- data.frame(
  variable = names(zero_props),
  pct_zero = zero_props
)
zero_df <- zero_df[zero_df$pct_zero > 0, ]

ggplot(zero_df, aes(x = reorder(variable, pct_zero), y = pct_zero)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Percentage of Zero Values by Variable",
       x = "Variable", y = "Percentage of Zeros (%)") +
  geom_text(aes(label = paste0(round(pct_zero, 1), "%")),
            hjust = -0.1, size = 3.5) +
  ylim(0, 105)
ggsave("zero_inflation.png", width = 8, height = 5)

#CORRELATION MATRIX 
cor_vars <- model_data_log[, c("log_tot_kg", "log_ammonia",
                               "log_refineries", "log_metals",
                               "log_ld_fcev", "log_mhd_fcev",
                               "log_seasonal", "log_area")]
cor_matrix <- cor(cor_vars, use = "complete.obs")

png("correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         number.cex = 0.7,
         mar = c(0, 0, 2, 0))
dev.off()

#SPATIAL MAP 
spatial_data <- data.frame(
  state  = tolower(data$state_name),
  tot_kg = data$tot_kg
)

state_data <- aggregate(tot_kg ~ state, 
                        data = spatial_data, 
                        FUN = mean)

us_map <- map_data("state")
map_merged <- merge(us_map, state_data,
                    by.x = "region",
                    by.y = "state")
map_merged <- map_merged[order(map_merged$order), ]

ggplot(map_merged, aes(x = long, y = lat,
                       group = group, 
                       fill = tot_kg / 1e6)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "lightyellow", 
                      high = "darkred",
                      name = "Mean Demand\n(millions kg)") +
  coord_fixed(1.3) +
  labs(title = "Mean Total Hydrogen Demand by State",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank())
ggsave("spatial_map.png", width = 10, height = 6)
