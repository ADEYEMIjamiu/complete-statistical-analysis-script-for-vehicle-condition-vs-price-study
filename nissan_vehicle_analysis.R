req_pkgs <- c("tidyverse", "broom", "patchwork", "jsonlite", "car", "emmeans")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
data_path <- "nissan-dataset.csv"
out_dir <- "figs"
if (!dir.exists(out_dir)) dir.create(out_dir)
list.files()
if (!file.exists(data_path)) 
  {
  stop(paste("File not found:", data_path, "\nEnsure nissan-dataset.csv is in working directory."))
}
getwd()
list.files()
file.exists("nissan-dataset.csv")
data_path <- "C:/Users/nurul/Documents/mtcar/nissan-dataset.csv"
nissan_raw <- readr::read_csv(data_path, show_col_types = FALSE)
library(tidyverse)
data1 <- read.csv(file.choose(), header = T)
nissan_raw <- data1
head(nissan_raw)
str(nissan_raw)
names(nissan_raw)
nissan_data <- nissan_raw %>%
  select(id, full_name, age, gender, model, color, performance, km, condition, price) %>%
  mutate(
    age = as.numeric(age),
    performance = as.numeric(performance),
    km = as.numeric(km),
    price = as.numeric(price)
  )
nissan_data <- nissan_raw
nissan_data$age <- as.numeric(nissan_data$age)
nissan_data$performance <- as.numeric(nissan_data$performance)
nissan_data$km <- as.numeric(nissan_data$km)
nissan_data$price <- as.numeric(nissan_data$price)
str(nissan_data)
table(nissan_data$condition)
summary(nissan_data$price)
nissan_clean <- nissan_data[!is.na(nissan_data$price) & !is.na(nissan_data$condition), ]
nrow(nissan_clean)
aggregate(price ~ condition, data = nissan_clean, FUN = mean)
aggregate(price ~ condition, data = nissan_clean, FUN = length)
price_by_condition <- aggregate(price ~ condition, data = nissan_clean, FUN = mean)
price_by_condition
price_by_condition <- aggregate(price ~ condition, data = nissan_clean, FUN = mean)
print(price_by_condition)
anova_result <- aov(price ~ condition, data = nissan_clean)
summary(anova_result)
boxplot(price ~ condition, data = nissan_clean, main = "Vehicle Price by Condition", xlab = "Condition", ylab = "Price")
print(price_by_condition[order(price_by_condition$price, decreasing = TRUE), ])
TukeyHSD(anova_result)
anova_summary <- summary(anova_result)
eta_squared <- anova_summary[[1]][1,2] / (anova_summary[[1]][1,2] + anova_summary[[1]][2,2])
cat("Effect size (eta-squared):", round(eta_squared, 4))
anova_summary <- summary(anova_result)
print(anova_summary)
eta_squared <- 2.662e+09 / (2.662e+09 + 1.691e+12)
cat("Effect size (eta-squared):", round(eta_squared, 4))
cat("=====================================\n")
cat("FINAL STATISTICAL ANALYSIS RESULTS\n") 
cat("=====================================\n")
cat("Hypothesis Test: One-way ANOVA\n")
cat("Sample size:", nrow(nissan_clean), "vehicles\n")
cat("F-statistic: 2.401\n")
cat("p-value: 0.0255\n")
cat("Significance level: 0.05\n")
cat("Result: REJECT null hypothesis\n")
cat("Conclusion: Vehicle condition DOES significantly affect price\n")
cat("BUT: Original business hypothesis is NOT supported\n")
cat("=====================================\n")
cat("=====================================\n")
cat("ANOVA ASSUMPTIONS TESTING\n")
cat("=====================================\n")
# 1. Test normality of residuals (Shapiro-Wilk on sample due to large n)
set.seed(123)
residual_sample <- sample(residuals(anova_result), 5000)
normality_test <- shapiro.test(residual_sample)
print(normality_test)

# 2. Test homogeneity of variances (Bartlett's test)
homogeneity_test <- bartlett.test(price ~ condition, data = nissan_clean)
print(homogeneity_test)

# 3. Visual assumption checks
par(mfrow = c(2,2))
plot(anova_result)

cat("ASSUMPTION VIOLATION DETECTED:\n")
cat("Bartlett test p-value:", format.pval(6.244e-16), "\n")
cat("Homogeneity assumption violated - unequal variances across groups\n")
cat("Recommendation: Use Welch's ANOVA (robust to unequal variances)\n")


# Welch's ANOVA (doesn't assume equal variances)
welch_anova <- oneway.test(price ~ condition, data = nissan_clean, var.equal = FALSE)
print(welch_anova)

# More comprehensive descriptive statistics
aggregate(cbind(price, km, performance) ~ condition, data = nissan_clean, FUN = function(x) c(mean = mean(x), sd = sd(x)))


# Check relationships between price and other continuous variables
cat("CORRELATION ANALYSIS:\n")
cat("Price vs Mileage (km):", round(cor(nissan_clean$price, nissan_clean$km, use = "complete.obs"), 3), "\n")
cat("Price vs Performance:", round(cor(nissan_clean$price, nissan_clean$performance, use = "complete.obs"), 3), "\n")
cat("Mileage vs Performance:", round(cor(nissan_clean$km, nissan_clean$performance, use = "complete.obs"), 3), "\n")

cat("\n=====================================\n")
cat("COMPLETE STATISTICAL ANALYSIS SUMMARY\n")
cat("=====================================\n")
cat("Original ANOVA: F = 2.401, p = 0.0255\n") 
cat("Welch ANOVA: F = 2.4, p = 0.02748\n")
cat("Both tests significant: Vehicle condition affects price\n")
cat("Effect size: Very small (eta-squared = 0.0016)\n")
cat("Assumption violations: Unequal variances detected\n")
cat("Robust test confirms: Results are reliable\n")
cat("Business conclusion: Condition categories don't follow expected premium pattern\n")

cat("CORRELATION ANALYSIS:\n")
cat("Price vs Mileage (km):", round(cor(nissan_clean$price, nissan_clean$km, use = "complete.obs"), 3), "\n")
cat("Price vs Performance:", round(cor(nissan_clean$price, nissan_clean$performance, use = "complete.obs"), 3), "\n")
cat("Mileage vs Performance:", round(cor(nissan_clean$km, nissan_clean$performance, use = "complete.obs"), 3), "\n")
