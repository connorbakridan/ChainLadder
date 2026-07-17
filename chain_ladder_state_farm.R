# chain_ladder_state_farm.R
#
# Estimates outstanding claims reserves (IBNR) for State Farm's Private
# Passenger Auto liability line using the Mack chain ladder method, on
# real claims data from the CAS Loss Reserving Database (NAIC Schedule P).
#
# Source: Casualty Actuarial Society, "Loss Reserving Data Pulled from
# NAIC Schedule P", PP Auto dataset.
# https://www.casact.org/publications-research/research/research-resources/loss-reserving-data-pulled-naic-schedule-p
#
# Unlike a typical reserving exercise, this dataset includes BOTH the
# upper triangle (what would have been known as of the 2007 evaluation
# date) and the true future values that later became known. That lets
# us fit the model on only the upper triangle, then check the projection
# against what actually happened -- a genuine out-of-sample validation,
# not just a fitted-in-sample result.

if (!require(ChainLadder)) install.packages("ChainLadder")
if (!require(openxlsx)) install.packages("openxlsx")
library(ChainLadder)
library(openxlsx)

# 1. Load and filter to State Farm -------------------------------------
# GRCODE 1767 = State Farm Mut Grp, the largest insurer in this dataset
# by a wide margin (~$18bn net earned premium vs ~$3bn for the next
# largest), chosen for a stable, well-populated triangle.
raw <- read.csv("ppauto_pos98-07.csv")
sf <- raw[raw$GRCODE == 1767, ]

cat("State Farm rows:", nrow(sf), "\n")
cat("Accident years:", paste(sort(unique(sf$AccidentYear)), collapse = ", "), "\n\n")

# 2. Split into "known as of 2007" vs "full" (includes real future values)
# DevelopmentYear = AccidentYear + DevelopmentLag - 1, so a cell was
# known as of the 2007 evaluation date if DevelopmentYear <= 2007.
sf$known_as_of_2007 <- sf$DevelopmentYear <= 2007

upper <- sf[sf$known_as_of_2007, ]
cat("Upper triangle cells (should be 55):", nrow(upper), "\n\n")

# 3. Build the triangle from the upper (known-only) data ----------------
# as.triangle() expects long-format origin/dev/value columns.
paid_triangle <- as.triangle(
  upper,
  origin = "AccidentYear",
  dev = "DevelopmentLag",
  value = "CumPaidLoss"
)

cat("Upper (known-as-of-2007) cumulative paid loss triangle:\n")
print(paid_triangle)
cat("\n")

# 4. Fit the Mack chain ladder model -------------------------------------
mack_model <- MackChainLadder(paid_triangle, est.sigma = "Mack")
mack_summary <- summary(mack_model)

cat("Mack Chain Ladder summary:\n")
print(mack_summary)
cat("\n")

# 5. Validate against what ACTUALLY happened -----------------------------
# The full dataset (not masked) contains the true ultimate paid losses at
# development lag 10 for every accident year -- these were genuinely
# unknown as of the 2007 evaluation date, so this is a real check of
# whether the model's projection was any good, not a circular comparison.
full_triangle <- as.triangle(
  sf,
  origin = "AccidentYear",
  dev = "DevelopmentLag",
  value = "CumPaidLoss"
)
actual_ultimate <- full_triangle[, "10"]

by_origin <- mack_summary$ByOrigin
by_origin$ActualUltimate <- actual_ultimate[rownames(by_origin)]
by_origin$Diff <- by_origin$Ultimate - by_origin$ActualUltimate
by_origin$PctDiff <- round(100 * by_origin$Diff / by_origin$ActualUltimate, 3)

cat("Projected ultimate vs what actually happened (known now, unknown at the time):\n")
print(by_origin[, c("Latest", "Ultimate", "ActualUltimate", "Diff", "PctDiff")])
cat("\n")

total_actual   <- sum(actual_ultimate)
total_projected <- sum(by_origin$Ultimate)
cat(sprintf(
  "Total actual ultimate: %.0f | Total projected ultimate: %.0f | Overall diff: %.2f%%\n\n",
  total_actual, total_projected, 100 * (total_projected - total_actual) / total_actual
))

# 6. Compare against the company's own posted reserve ---------------------
# PostedReserves2007 is State Farm's own reported total reserve position
# as of the 2007 evaluation date (across all accident years combined for
# this line). This is NOT the same basis as our paid-loss-only chain
# ladder reserve (it likely reflects case reserves plus IBNR margin, not
# just development on paid losses), so a gap here is expected and worth
# discussing rather than treated as a discrepancy to explain away.
posted_reserve <- unique(sf$PostedReserves2007)
model_reserve  <- sum(by_origin$Ultimate - by_origin$Latest)

cat(sprintf(
  "Chain ladder projected total reserve: %.0f\nCompany posted reserve (2007): %.0f\nDifference: %.0f (%.1f%%)\n\n",
  model_reserve, posted_reserve, posted_reserve - model_reserve,
  100 * (posted_reserve - model_reserve) / posted_reserve
))

# 7. Clean and export results --------------------------------------------
origin_df <- as.data.frame(mack_summary$ByOrigin)
totals_df <- as.data.frame(mack_summary$Totals)
origin_df[] <- lapply(origin_df, function(x) ifelse(is.nan(x) | is.infinite(x), "", x))
totals_df[] <- lapply(totals_df, function(x) ifelse(is.nan(x) | is.infinite(x), "", x))

write.xlsx(list(
  "ByOrigin" = origin_df,
  "Totals" = totals_df,
  "Validation" = by_origin
), file = "state_farm_chainladder_results.xlsx")

cat("Exported results to 'state_farm_chainladder_results.xlsx'.\n")

suppressWarnings(plot(mack_model, which = 1))
suppressWarnings(plot(mack_model, which = 2))
