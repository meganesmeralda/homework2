library(dplyr)
library(ggplot2)

# Count the number of hospitals with multiple reports per year
multi_report_counts <- duplicate.hcris %>%
  group_by(fyear) %>%
  summarise(num_hospitals = n_distinct(provider_number))

# Plot the number of hospitals filing multiple reports by year
ggplot(multi_report_counts, aes(x = fyear, y = num_hospitals)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Hospitals Filing Multiple Reports Per Year",
    x = "Year",
    y = "Number of Hospitals",
    caption = "Source: HCRIS Data (1996 & 2010 Versions)"
  ) +
  theme_minimal()

# Count the number of unique hospital IDs
unique_hospital_count <- final.hcris.data %>%
  distinct(provider_number) %>%
  nrow()

# Print the result
cat("Number of unique hospital IDs (Medicare provider numbers):", unique_hospital_count, "\n")

# Create violin plot of total charges by year
ggplot(final.hcris.data, aes(x = factor(year), y = tot_charges)) +
  geom_violin(fill = "lightblue", color = "darkblue", alpha = 0.6) +
  scale_y_log10() +  # Apply log scale to handle skewed distributions
  labs(
    title = "Distribution of Total Charges by Year",
    x = "Year",
    y = "Total Charges (log scale)",
    caption = "Source: HCRIS Data (1996 & 2010 Versions)"
  ) +
  theme_minimal()

# Step 1: Calculate discount factor
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = if_else(price_denom > 0, price_num / price_denom, NA_real_) # Avoid division by zero or negative denominator
  )

# Step 2: Filter out negative or invalid prices and any other outliers (e.g., top 1% of prices)
final.hcris.data_clean <- final.hcris.data %>%
  filter(!is.na(price) & price > 0)

# Step 3: Create a violin plot to show the distribution of estimated prices by year
ggplot(final.hcris.data_clean, aes(x = as.factor(year), y = price)) +
  geom_violin(trim = TRUE, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Estimated Prices by Year",
    x = "Year",
    y = "Estimated Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate price and define penalty
final.hcris.data <- final.hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  )

# Filter for 2012 and define penalty
final.hcris.2012 <- final.hcris.data %>%
  ungroup() %>%
  filter(
    price_denom > 100, !is.na(price_denom),
    price_num > 0, !is.na(price_num),
    price < 100000,
    beds > 30,
    year == 2012
  ) %>%
  mutate(
    hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
    hrrp_payment = ifelse(is.na(hrrp_payment), 0, abs(hrrp_payment)),
    penalty = (hvbp_payment - hrrp_payment) < 0  # TRUE/FALSE
  )
# Calculate mean prices for penalized vs non-penalized hospitals
mean.pen <- round(mean(final.hcris.2012$price[final.hcris.2012$penalty == TRUE], na.rm = TRUE), 2)
mean.nopen <- round(mean(final.hcris.2012$price[final.hcris.2012$penalty == FALSE], na.rm = TRUE), 2)

# Print results
cat("Mean price for penalized hospitals:", mean.pen, "\n")
cat("Mean price for non-penalized hospitals:", mean.nopen, "\n")

# Define penalty: HVBP + HRRP < 0
final.hcris.2012 <- final.hcris %>%
  mutate(
    hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
    hrrp_payment = ifelse(is.na(hrrp_payment), 0, hrrp_payment),
    penalty = (hvbp_payment + hrrp_payment) < 0
  )

# Calculate bed size quartiles
bed_quartiles <- quantile(final.hcris.2012$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Assign each hospital to a bed size quartile
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(
    Q1 = ifelse(beds <= bed_quartiles[1], 1, 0),
    Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
    Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
    Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
  )

  # Read in the dataset
hcris.data <- read_rds("data/output/HCRIS_Data_v2010.rds")

# Calculate price and define penalty
hcris.data <- hcris.data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  )

# Filter for 2012 and define penalty
final.hcris <- hcris.data %>%
  ungroup() %>%
  filter(
    price_denom > 100, !is.na(price_denom),
    price_num > 0, !is.na(price_num),
    price < 100000,
    beds > 30,
    year == 2012
  ) %>%
  mutate(
    hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
    hrrp_payment = ifelse(is.na(hrrp_payment), 0, abs(hrrp_payment)),
    penalty = (hvbp_payment - hrrp_payment) < 0  # TRUE/FALSE
  )
# Calculate mean prices for penalized vs non-penalized hospitals
mean.pen <- round(mean(final.hcris$price[final.hcris$penalty == TRUE], na.rm = TRUE), 2)
mean.nopen <- round(mean(final.hcris$price[final.hcris$penalty == FALSE], na.rm = TRUE), 2)

# Print results
cat("Mean price for penalized hospitals:", mean.pen, "\n")
cat("Mean price for non-penalized hospitals:", mean.nopen, "\n")

# Calculate quartiles of bed size
bed_quartiles <- quantile(final.hcris$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Add quartile indicators
final.hcris <- final.hcris %>%
  mutate(
    Q1 = ifelse(beds <= bed_quartiles[1], 1, 0),
    Q2 = ifelse(beds > bed_quartiles[1] & beds <= bed_quartiles[2], 1, 0),
    Q3 = ifelse(beds > bed_quartiles[2] & beds <= bed_quartiles[3], 1, 0),
    Q4 = ifelse(beds > bed_quartiles[3], 1, 0)
  )
# Calculate average price by penalty and quartile
quartile_summary <- final.hcris %>%
  mutate(quartile = case_when(
    Q1 == 1 ~ "Q1",
    Q2 == 1 ~ "Q2",
    Q3 == 1 ~ "Q3",
    Q4 == 1 ~ "Q4"
  )) %>%
  group_by(quartile, penalty) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  arrange(quartile, penalty)

# Display the table
quartile_summary_table <- quartile_summary %>%
  tidyr::pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

print(quartile_summary_table)

# Calculate average prices by quartile and penalty status
quartile_summary <- final.hcris.2012 %>%
  mutate(bed_quartile = case_when(
    Q1 == 1 ~ "Q1",
    Q2 == 1 ~ "Q2",
    Q3 == 1 ~ "Q3",
    Q4 == 1 ~ "Q4"
  )) %>%
  group_by(bed_quartile, penalty) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = penalty, values_from = avg_price, names_prefix = "penalty_")

# Print the table
print(quartile_summary)

# Find the average treatment effect using each of the following estimators, and present your results in a single table:

## Nearest neighbor matching (1-to-1) with inverse variance distance based on quartiles of bed size

 install.packages("MatchIt")    # For nearest neighbor matching
install.packages("WeightIt")    # For inverse propensity weighting
install.packages("broom")       # For cleaning model outputs
install.packages("sandwich")    # For robust standard errors
install.packages("lmtest")
install.packages("Matching")
library(dplyr)
library(MatchIt)
library(WeightIt)
library(broom)
library(sandwich)
library(lmtest)

# Define lp.covs with the relevant covariates
lp.covs <- final.hcris.2012 %>%
  select(beds, medicaid_discharges, inpatient_charges, medicare_discharges, medicare_payments) %>%
  as.matrix()

m.nn.var <- Matching::Match(Y=final.hcris.2012$price,
                            Tr=final.hcris.2012$penalty,
                            X=lp.covs,
                            M=4,  #<< 4 nearest neighbbors 
                            Weight=1,
                            estimand="ATE")

v.name=final.hcris.2012(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments")) 

