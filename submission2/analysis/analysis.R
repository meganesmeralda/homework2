# the goods to start off
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(png)

# Question 1: Hospital Reports
## gathering the data
report_counts <- duplicate.hcris %>%
    group_by(fyear) %>%
    summarise(num_hospitals = n_distinct(provider_number))

## creating a line graph
table1 = ggplot(report_counts, aes(x = fyear, y = num_hospitals)) +
        geom_line() +
        labs(title = "Number of Hospitals Over Time",
                         x = "Fiscal Year",
                         y = "Number of Hospitals") +
            theme_minimal()

dup.count <- final.hcris %>%
    add_count

ggsave("submission2/Q1.png", plot = table1, width = 6, height = 5, dpi = 300, bg = "white")

# Question 2: Unique Hospital IDs
unique_hospital_count <- length(unique(final.hcris.data$provider_number))
print(unique_hospital_count)
# 6747

#Question 2, anotha one
data=read_rds("data/output/HCRIS_Data.rds")
unique_counts <- data %>%
  group_by(year) %>%
  summarise(num_unique_providers = n_distinct(provider_number), .groups = 'drop')


library(flextable)

# Create a nice table
table <- unique_counts %>%
  flextable() %>%
  set_caption("Unique Providers Per Year") %>%
  theme_vanilla() %>%
  bg(part = "all", bg = "white") %>%
  set_table_properties(width = 1, layout = "autofit") %>% # Auto-fit columns
  font(fontname = "Arial") %>% # Set a nice readable font
  align(align = "center", part = "all") # Center text

# Save as PNG
install.packages("webshot2") # Install webshot2 for saving images
library("webshot2")
save_as_image(table, path = "unique_providers_per_year.png")

# Question 3: Distribution of Total Charges by Year
final.hcris.data$fyear <- as.factor(final.hcris.data$year)
table2 = ggplot(final.hcris.data, aes(x = fyear, y = log(tot_charges))) + geom_violin(fill = "lightblue", color = "darkblue") + labs(title = "Log-transformed Distribution of Total Charges by Year", x = "Year", y = "Log of Total Charges" ) +theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("submission2/Q3.png", plot = table1, width = 6, height = 5, dpi = 300, bg = "white")


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

table4 = ggplot(final.hcris.data, aes(x = fyear, y = price)) +
    geom_violin(fill = "lightblue", color = "darkblue") +  
    labs(
        title = "Distribution of Estimated Prices by Year",
        x = "Year",
        y = "Estimated Price"
    ) +
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("submission2/Q4.png", plot = table1, width = 6, height = 5, dpi = 300, bg = "white")

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

# Save the workspace with all the answers to each of the questions
save(report_counts, unique_hospital_count, final.hcris.data, final.hcris.2012, mean.pen, mean.nopen, results, quartile_summary, file = "submission2/analysis_workspace.Rdata")
