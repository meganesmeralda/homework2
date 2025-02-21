# downloading libraries
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(png)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

hcris.data <- readRDS("data/output/HCRIS_Data.rds")


# Question 1: Hospital Reports
## gathering the data
report_counts <- duplicate.hcris %>%
    group_by(fyear) %>%
    summarise(num_hospitals = n_distinct(provider_number))

## creating a line graph
q1 = ggplot(report_counts, aes(x = fyear, y = num_hospitals)) +
        geom_line() +
        labs(title = "Number of Hospitals Over Time",
                         x = "Fiscal Year",
                         y = "Number of Hospitals") +
            theme_minimal()

# Question 2: Unique Hospital IDs
unique_hospital_count <- length(unique(final.hcris.data$provider_number))
print(unique_hospital_count)

unique_hospital_count <- final.hcris.data %>%
  distinct(provider_number) %>%
  nrow()


# Question 3: Distribution of Total Charges by Year
q3 = ggplot(final.hcris.data, aes(x = fyear, y = log(tot_charges))) +
  geom_violin(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Log-transformed Distribution of Total Charges by Year",
    x = "Year",
    y = "Log of Total Charges"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Question 4: Distribution of Estimated Prices in Each Year
final.hcris.data <- final.hcris.data %>%
    mutate(discount_factor = 1 - tot_discounts / tot_charges)

final.hcris.data <- final.hcris.data %>%
    mutate(price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment)

final.hcris.data <- final.hcris.data %>%
    mutate(price_denom = tot_discharges - mcare_discharges)

final.hcris.data <- final.hcris.data %>%
    mutate(price = price_num / price_denom)

quantiles <- quantile(final.hcris.data$price, c(0.01, 0.99), na.rm = TRUE)
final.hcris.data <- final.hcris.data %>%
    filter(price >= quantiles[1], price <= quantiles[2])

final.hcris.data$fyear <- as.factor(final.hcris.data$fyear)

q4 = ggplot(final.hcris.data, aes(x = fyear, y = price)) +
    geom_violin(fill = "lightblue", color = "darkblue") +  
    labs(
        title = "Distribution of Estimated Prices by Year",
        x = "Year",
        y = "Estimated Price"
    ) +
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

final.hcris.2012 <- final.hcris.data %>% ungroup() %>%
  filter(price_denom > 100, !is.na(price_denom), 
         price_num > 0, !is.na(price_num),
         price < 100000, 
         beds > 30, year == 2012) %>%
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment), 0, abs(hrrp_payment)), 
    penalty = (hvbp_payment - hrrp_payment < 0))

# Question 5: 
## Filter for 2012 and define penalty
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
## Calculate mean prices for penalized vs non-penalized hospitals
mean.pen <- round(mean(final.hcris.2012$price[final.hcris.2012$penalty == TRUE], na.rm = TRUE), 2)
mean.nopen <- round(mean(final.hcris.2012$price[final.hcris.2012$penalty == FALSE], na.rm = TRUE), 2)

## Print results
cat("Mean price for penalized hospitals:", mean.pen, "\n")
cat("Mean price for non-penalized hospitals:", mean.nopen, "\n")

# Print results as a table
results <- data.frame(
    Category = c("Penalized Hospitals", "Non-Penalized Hospitals"),
    Mean_Price = c(mean.pen, mean.nopen)
)

print(results)
## p=9685, np=9323

# Question 6: Hospitals into quartiles
## Define penalty: HVBP + HRRP < 0
final.hcris.2012 <- final.hcris.data %>%
    mutate(
        hvbp_payment = ifelse(is.na(hvbp_payment), 0, hvbp_payment),
        hrrp_payment = ifelse(is.na(hrrp_payment), 0, hrrp_payment),
        penalty = (hvbp_payment + hrrp_payment) < 0
    )
## Calculate bed size quartiles
bed_quartiles <- quantile(final.hcris.2012$beds, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

## Assign each hospital to a bed size quartile
final.hcris.2012 <- final.hcris.2012 %>%
  mutate(
    Q1 = as.numeric((beds <= bed_quartiles[1]) & (beds > 0)),
    Q2 = as.numeric((beds > bed_quartiles[1]) & (beds <= bed_quartiles[2])),
    Q3 = as.numeric((beds > bed_quartiles[2]) & (beds <= bed_quartiles[3])),
    Q4 = as.numeric(beds > bed_quartiles[3])
  )

## Calculate average prices by quartile and penalty status
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

## Print the table
print(quartile_summary)

# Question 7: Average Treatment Effect Estimation
# Load necessary libraries
library(MatchIt)
library(survey)

# Prepare data for matching
final.hcris.2012 <- final.hcris.2012 %>%
    mutate(bed_quartile = case_when(
        Q1 == 1 ~ "Q1",
        Q2 == 1 ~ "Q2",
        Q3 == 1 ~ "Q3",
        Q4 == 1 ~ "Q4"
    ))

# Nearest neighbor matching (1-to-1) with inverse variance distance
match_iv <- matchit(penalty ~ bed_quartile, data = final.hcris.2012, method = "nearest", distance = "logit")
matched_data_iv <- match.data(match_iv)
ate_iv <- with(matched_data_iv, mean(price[penalty == TRUE]) - mean(price[penalty == FALSE]))

# Nearest neighbor matching (1-to-1) with Mahalanobis distance
match_mahal <- matchit(penalty ~ bed_quartile, data = final.hcris.2012, method = "nearest", distance = "mahalanobis")
matched_data_mahal <- match.data(match_mahal)
ate_mahal <- with(matched_data_mahal, mean(price[penalty == TRUE]) - mean(price[penalty == FALSE]))

# Inverse propensity weighting
ps_model <- glm(penalty ~ bed_quartile, data = final.hcris.2012, family = binomial)
final.hcris.2012$ps <- predict(ps_model, type = "response")
final.hcris.2012$weight <- ifelse(final.hcris.2012$penalty == TRUE, 1 / final.hcris.2012$ps, 1 / (1 - final.hcris.2012$ps))
design <- svydesign(ids = ~1, weights = ~weight, data = final.hcris.2012)
ate_ipw <- svymean(~price, design, subset = penalty == TRUE) - svymean(~price, design, subset = penalty == FALSE)

# Simple linear regression
lm_model <- lm(price ~ penalty * bed_quartile, data = final.hcris.2012)
ate_lm <- coef(lm_model)["penaltyTRUE"]

# Present results in a table
ate_results <- data.frame(
    Estimator = c("Nearest Neighbor (Inverse Variance)", "Nearest Neighbor (Mahalanobis)", "Inverse Propensity Weighting", "Linear Regression"),
    ATE = c(ate_iv, ate_mahal, ate_ipw, ate_lm)
)

print(ate_results)

# Save the workspace with all the answers to each of the questions
save(report_counts, unique_hospital_count, final.hcris.data, final.hcris.2012, mean.pen, mean.nopen, results, quartile_summary, file = "submission2/analysis_workspace.Rdata")
