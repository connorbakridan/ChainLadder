# 1. Load Required Packages
if (!require(ChainLadder)) install.packages("ChainLadder")
if (!require(openxlsx)) install.packages("openxlsx")
library(ChainLadder)
library(openxlsx)

# 2. Create a Proper Claims Triangle
my_data <- matrix(c(
  3000, 4500, 5700, 6700, 7400,
  3200, 4700, 5900, 7000,   NA,
  3400, 4900, 6100,   NA,   NA,
  3600, 5100,   NA,   NA,   NA,
  3800,   NA,   NA,   NA,   NA
), nrow = 5, byrow = TRUE)

colnames(my_data) <- paste0("Dev", 1:5)
rownames(my_data) <- paste0("Origin", 2019:2023)
my_triangle <- as.triangle(my_data)

# 3. Mack Chain-Ladder
my_model <- MackChainLadder(my_triangle, est.sigma = "Mack")
summary_my_model <- summary(my_model)

# 4. Clean any NaN/Inf before export
origin_df <- as.data.frame(summary_my_model$ByOrigin)
totals_df <- as.data.frame(summary_my_model$Totals)

# Replace NaN/Inf with blank ("") for Excel compatibility
origin_df[] <- lapply(origin_df, function(x) ifelse(is.nan(x) | is.infinite(x), "", x))
totals_df[] <- lapply(totals_df, function(x) ifelse(is.nan(x) | is.infinite(x), "", x))

# 5. Export results to Excel (.xlsx)
write.xlsx(list(
  "ByOrigin" = origin_df,
  "Totals" = totals_df
), file = "mack_chainladder_results_cleaned.xlsx")

cat("\nExported cleaned results to 'mack_chainladder_results_cleaned.xlsx'.\n")

# (Optional) Plots, suppressing warnings for a clean run
suppressWarnings(plot(my_model, which = 1))
suppressWarnings(plot(my_model, which = 2))


