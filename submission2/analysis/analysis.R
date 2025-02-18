# the goods to start off
library(dplyr)
library(ggplot2)

# Question 1: Hospital Reports
## gathering the data
report_counts <- duplicate.hcris %>%
  group_by(fyear) %>%
  summarise(num_hospitals = n_distinct(provider_number))

## creating a line graph
ggplot(report_counts, aes(x = fyear, y = num_hospitals)) +
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
  # 48803


