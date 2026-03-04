#####################################
# Statistical Programming with R
# Capstone Project
# U.S. Business Formation (2021–2024)
#  Tinashe Chisango
#####################################

# Instructions:
# 1. Open BAN663_Capstone_Project.Rproj in RStudio.
# 2. Then open "Group6_FinalProject.R" and Run All.
# 3. The script assumes bfs_monthly.csv is in the same folder.

#####################################

# Clear console
cat("\014")

# Clear environment
rm(list = ls())

# Libraries (course core)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Data Introduction  
# Load data
bfs <- read_csv(here("bfs_monthly.csv"), show_col_types = FALSE)

# Clean Data
# Check structure and overview of raw data
str(bfs)
summary(bfs)
colnames(bfs)
# There is both numeric and non-numeric data

# Filter: states + DC, TOTAL industry, non-seasonally adjusted, 2021–2024
# The data needs to be filtered to just the information needed for the project
# The original dataset had 35190 rows of data, including back to 2004 and with extra area information (exclude US, NO, MW, SO, WE, PR)
# Did this at the beginning so that the following cleaning was not done on all of the unnecessary data (extra work)
# Used a filter, then saved as a new df, to make sure future work was done on the correct data

# Make a frame for which state abbreviations need to be kept in the cleaned df
state_abbr <- c(state.abb, "DC")

# Filter
bfs_clean <- bfs %>%
  filter(
    sa == "U",
    naics_sector == "TOTAL",
    geo %in% state_abbr,
    year >= 2021, year <= 2024
  ) %>%
  distinct() # Remove duplicate rows

# Inspect for special values
# NA
na_count <- sapply(bfs_clean, function(x) sum(is.na(x)))
print(na_count)
# Inf
inf_count <- sapply(bfs_clean, function(x) sum(is.infinite(x)))
print(inf_count)
# NaN
nan_count <- sapply(bfs_clean, function(x) sum(is.nan(x)))
print(nan_count)
# Check for duplicates
sum(duplicated(bfs_clean))

# Remove special values
bfs_clean <- bfs_clean %>%
  distinct() %>%
  filter(
    if_all(everything(), ~ !is.na(.) &
             !is.nan(.) &
             !is.infinite(.))
  )

# Confirm data types & structure are correct, inspect first few rows, list unique series and state codes
str(bfs_clean)
head(bfs_clean)
unique(bfs_clean$series)
unique(bfs_clean$geo)

# Reshape and build date 
# Define the month order for plots and for building dates, in a list format
month_levels <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

# Create new date form based on the month columns in the original dataset
# This makes it easier to work with the dates and perform analysis on them, also makes labels on visuals clearer
bfs_long <- bfs_clean %>%
  pivot_longer(jan:dec, names_to = "month", values_to = "count") %>% # turns the 12 month columns into two columns: month and count
  mutate(
    month = factor(month, levels = month_levels), # makes month an ordered factor
    count = as.numeric(count), # makes count numeric
    month_num = match(month, month_levels), # converts month to a number
    date = as.Date(sprintf("%d-%02d-01", year, month_num)) # builds a real date for each month
  ) %>%
  filter(!is.na(count)) %>%
  select(-month_num)

# Latest month indicator (used in subtitles of charts & graphs)
latest_date <- max(bfs_long$date, na.rm = TRUE)

# Create series labels that make each code in the series column into a more understandable name
# Note: we use BF_SBF8Q to represent employer formations because it directly measures the number of new employer identifying businesses that formed (and more importantly actually started hiring employees)
bfs_long <- bfs_long %>%
  mutate(series_label = dplyr::case_when(
    series == "BA_BA"    ~ "Total Business Applications",
    series == "BA_HBA"   ~ "High-Propensity Applications",
    series == "BA_WBA"   ~ "Applications with Planned Wages",
    series == "BA_CBA"   ~ "Corporate Applications",
    series == "BF_SBF8Q" ~ "Employer Formations (SBF, 8Q)",
    series == "BF_BF4Q"  ~ "Projected Formations within 4Q",
    TRUE ~ series
  ))

# Exploratory Data Analysis (EDA)
# Examine summary statistics and missing values
summary(bfs_long)
colSums(is.na(bfs_long))

# Look at a few different variable comparisons
# Top states by record count
bfs_long %>%
  count(geo, sort = TRUE) %>%
  head(10)

# Top 10 states by total applications
bfs_long %>%
  filter(series == "BA_BA") %>% # filters to just total business applications
  group_by(geo) %>%
  summarise(total_apps = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_apps)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(geo, total_apps), y = total_apps)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 States by Total Business Applications (2021–2024)",
    x = "State",
    y = "Total Applications"
  ) +
  theme_minimal()

# Total business applications per year
bfs_total_year <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(year) %>%
  summarize(total_applications = sum(count, na.rm = TRUE))

# print results
bfs_total_year

# Plot the annual totals as a line
ggplot(bfs_total_year, aes(x = factor(year), y = total_applications, group = 1)) +
  geom_line(color = "grey", size = 1.2) +
  geom_point(color = "red", size = 3) +
  labs(title = "Total U.S. Business Applications (2021–2024)",
       x = "Year",
       y = "Number of Applications") +
  theme_minimal()

## Descriptive stats organized by state
# Descriptive statistics on business applications (BA_BA) per year and per month, organized by state
apps_month_year_stats <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(year, month) %>%                     
  summarise(
    n_states   = n_distinct(geo),
    total_us   = sum(count, na.rm = TRUE),      
    mean_state = mean(count, na.rm = TRUE),
    median_state = median(count, na.rm = TRUE),
    sd_state   = sd(count, na.rm = TRUE),
    q1_state   = quantile(count, 0.25, na.rm = TRUE),
    q3_state   = quantile(count, 0.75, na.rm = TRUE),
    iqr_state  = IQR(count, na.rm = TRUE),
    min_state  = min(count, na.rm = TRUE),
    max_state  = max(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, month)

print(apps_month_year_stats, n = Inf)

# Descriptive statistics on employer formations (BF_SBF8Q) per year and per month, organized by state
forms_month_year_stats <- bfs_long %>%
  filter(series == "BF_SBF8Q") %>%
  group_by(year, month) %>%
  summarise(
    n_states   = n_distinct(geo),
    total_us   = sum(count, na.rm = TRUE),
    mean_state = mean(count, na.rm = TRUE),
    median_state = median(count, na.rm = TRUE),
    sd_state   = sd(count, na.rm = TRUE),
    q1_state   = quantile(count, 0.25, na.rm = TRUE),
    q3_state   = quantile(count, 0.75, na.rm = TRUE),
    iqr_state  = IQR(count, na.rm = TRUE),
    min_state  = min(count, na.rm = TRUE),
    max_state  = max(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, month)

print(forms_month_year_stats, n = Inf)

## Descriptive states organized by months, not as much focus on states
# Build national monthly totals for applications
apps_us_monthly <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(year, month) %>%
  summarise(total_us = sum(count, na.rm = TRUE), .groups = "drop")

# Build national monthly totals for formations
forms_us_monthly <- bfs_long %>%
  filter(series == "BF_SBF8Q") %>%
  group_by(year, month) %>%
  summarise(total_us = sum(count, na.rm = TRUE), .groups = "drop")

# Descriptive statistics for applications per year, across the months
apps_year_summary <- apps_us_monthly %>%
  group_by(year) %>%
  summarise(
    mean_monthly = mean(total_us, na.rm = TRUE),
    median_monthly = median(total_us, na.rm = TRUE),
    sd_monthly   = sd(total_us, na.rm = TRUE),
    min_monthly  = min(total_us, na.rm = TRUE),
    q1_monthly   = quantile(total_us, 0.25, na.rm = TRUE),
    q3_monthly   = quantile(total_us, 0.75, na.rm = TRUE),
    iqr_monthly  = IQR(total_us, na.rm = TRUE),
    max_monthly  = max(total_us, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

print(apps_year_summary)

# Descriptive statistics for formations per year, across the months
forms_year_summary <- forms_us_monthly %>%
  group_by(year) %>%
  summarise(
    mean_monthly = mean(total_us, na.rm = TRUE),
    median_monthly = median(total_us, na.rm = TRUE),
    sd_monthly   = sd(total_us, na.rm = TRUE),
    min_monthly  = min(total_us, na.rm = TRUE),
    q1_monthly   = quantile(total_us, 0.25, na.rm = TRUE),
    q3_monthly   = quantile(total_us, 0.75, na.rm = TRUE),
    iqr_monthly  = IQR(total_us, na.rm = TRUE),
    max_monthly  = max(total_us, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

print(forms_year_summary)



### Case Study Question 1: What is the total count of new business applications per month for each year (2021–2024)? 
# Monthly totals by year (BA_BA)
# Sum BA_BA across all states for each month in each year
bfs_apps_monthly <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(year, month) %>%
  summarise(total_applications = sum(count, na.rm = TRUE), .groups = "drop")

## Answer: 
# print results for how many new business applications there were per month for each year
bfs_apps_monthly

# Plot a multi-line chart where each color represents a year and the months are along the x axis
ggplot(bfs_apps_monthly, aes(x = month, y = total_applications, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Total Business Applications by Year",
    subtitle = paste("Through", format(latest_date, "%B %Y")),
    x = "Month", y = "Applications (All States)", color = "Year"
  ) +
  theme_minimal()

## Overall conclusions: 
# Business applications show consistent seasonal peaks in spring, especially in the months of March, April, and May
# There are dips in business applications at the end of each year 
# The highest peak came in March 2023, which was just above March 2021
# 2021 seemed to see the highest monthly peaks following the pandemic surge, while 2022 dropped slightly before stabilizing through 2023 and 2024



### Case Study Question 2: What are the most common types of business filings (e.g., high-propensity, corporation, LLC)?
# Most common filing types (2021–2024)
# Total counts across 2021-24 for each filing type
bfs_filing_types <- bfs_long %>%
  group_by(series_label) %>%
  summarise(total_filings = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_filings))

## Answer: 
# print results for the breakdown of type of business filing
bfs_filing_types


# Horizontal bar chart of actual totals by type
ggplot(bfs_filing_types, aes(x = reorder(series_label, total_filings), y = total_filings)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Common Business Filing Types (2021–2024)",
    x = "Filing Type", y = "Total Count"
  ) +
  theme_minimal()

## Overall conclusions
# The highest total is obviously the total business applications, which collects all applications
# Next highest, so really the most common type, is high-propensity applications 
# Corporate and wage-related filings were much smaller
# Employer formations were only a fraction of total applications, this shows that many applications do not lead to employer businesses



### Case Study Question 3: What is the monthly count of employer-identifying business formations by year?
# Monthly employer formations by year (BF_SBF8Q)
# Sum employer-identifying formations across states by month/year
bfs_formations <- bfs_long %>%
  filter(series == "BF_SBF8Q") %>%
  group_by(year, month) %>%
  summarise(total_formations = sum(count, na.rm = TRUE), .groups = "drop")

## Answer: 
# print results to show the monthly count of employer-identifying business formations by year
bfs_formations

# Plot a multi-line chart for formations
ggplot(bfs_formations, aes(x = month, y = total_formations, group = factor(year), color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Employer-Identifying Business Formations (SBF 8Q)",
    subtitle = paste("Through", format(latest_date, "%B %Y")),
    x = "Month", y = "Formations (All States)", color = "Year"
  ) +
  theme_minimal()

## Overall conclusions:
# Employer formations followed the same seasonal trends as applications, with early-year peaks and late-year declines. 
# Activity dropped slightly in 2022, then steadied in 2023 before dropping slightly again in 2024



### Case Study Question 4: What are the percentage increases or decreases in applications and formations compared to the previous year, extending through the most recent data in 2024?
# YoY % change (applications + formations)
# Create df for year-over-year (YoY) rates 
yoy_df <- bfs_long %>%
  filter(series %in% c("BA_BA", "BF_SBF8Q")) %>%
  group_by(series, year) %>%
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
  group_by(series) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(yoy_change_pct = 100 * (total - lag(total)) / lag(total)) %>%
  ungroup()

# Line chart of YoY % by series
ggplot(yoy_df, aes(x = year, y = yoy_change_pct, color = series, group = series)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  labs(
    title = "Year-over-Year Change (%)",
    subtitle = "Total Applications (BA_BA) and Employer Formations (BF_SBF8Q)",
    x = "Year", y = "YoY Change (%)", color = "Series"
  ) +
  theme_minimal()

# Table with results
yoy_summary_table <- yoy_df %>%
  select(series, year, total, yoy_change_pct) %>%
  arrange(series, year)

## Answer: 
# Print formatted table, percentage changes are in the rightmost column. The first four rows represent the applications for each year, the last four rows represent the actual formations for each year
print(yoy_summary_table, n = Inf)

## Overall conclusions:
# Both applications and formations declined in 2022 after the 2021 boom, rebounded sharply in 2023, and softened again in 2024 (but not as deeply as from 2021 to 2022)
# The alternating increases and decreases indicate a lot of variation and change short-term, but there's no evidence of a sustained downward trend



### Case Study Question 5: What is the propensity (trend line) of increases or decreases in total business applications over time through 2024? 
# Trend of total applications over time
# Builds a national monthly time series of total applications
bfs_trend_app <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(date) %>%
  summarise(total_applications = sum(count, na.rm = TRUE), .groups = "drop")

# Plot the monthly time series
ggplot(bfs_trend_app, aes(x = date, y = total_applications)) +
  geom_line() +
  labs(
    title = "Trend: Total Business Applications",
    subtitle = paste("Jan 2021–", format(latest_date, "%b %Y")),
    x = "Date", y = "Applications (All States)"
  ) +
  theme_minimal()

# Show results, with each quarter's average/min/max monthly totals
bfs_trend_app_summary <- bfs_trend_app %>%
  mutate(year = as.integer(format(date, "%Y")),
         quarter = paste0("Q", ceiling(as.integer(format(date, "%m")) / 3))) %>%
  group_by(year, quarter) %>%
  summarise(
    avg_applications = mean(total_applications, na.rm = TRUE),
    min_applications = min(total_applications, na.rm = TRUE),
    max_applications = max(total_applications, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary
print(bfs_trend_app_summary, n = Inf)

## Overall conclusions:
# It seems like there is a pretty visible pattern of total applications each year with a peak in the beginning of the year followed by decreases
# It was interesting to see how similar each year's pattern was in shape
# Although there are fluctuations throughout each year, there is still consistent activity which is encouraging



### Case Study Question 6: Trend of high-propensity applications (BA_HBA) over time
# Monthly national series for high-propensity applications
bfs_trend_hba <- bfs_long %>%
  filter(series == "BA_HBA") %>%
  group_by(date) %>%
  summarise(total_hba = sum(count, na.rm = TRUE), .groups = "drop")

# Plot the monthly series
ggplot(bfs_trend_hba, aes(x = date, y = total_hba)) +
  geom_line() +
  labs(
    title = "Trend: High-Propensity Business Applications",
    subtitle = paste("Jan 2021–", format(latest_date, "%b %Y")),
    x = "Date", y = "High-Propensity Applications"
  ) +
  theme_minimal()

# Show results
bfs_trend_hba_summary <- bfs_trend_hba %>%
  mutate(year = as.integer(format(date, "%Y")),
         quarter = paste0("Q", ceiling(as.integer(format(date, "%m")) / 3))) %>%
  group_by(year, quarter) %>%
  summarise(
    avg_highprop = mean(total_hba, na.rm = TRUE),
    min_highprop = min(total_hba, na.rm = TRUE),
    max_highprop = max(total_hba, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary
print(bfs_trend_hba_summary, n = Inf)

## Overall conclusions:
# High-propensity applications, or those most likely to become employer formations, had very similar trends to those of the total applications
# This trend was a peak in the beginning of the year followed by decreases



### Hypothesis testing
# First Hypothesis
# High BA states vs lower BA states on mean monthly formations
# Null hypothesis (H₀): There is no difference in mean monthly employer formations between high-application states and lower-application states.
# Alternative hypothesis (H₁): High-application states have higher mean monthly employer formations than lower-application states.

# Total BA_BA per state across 2021-24
state_apps <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(geo) %>%
  summarise(total_apps = sum(count, na.rm = TRUE), .groups = "drop")

# Median state total
median_apps <- median(state_apps$total_apps, na.rm = TRUE)

# Label each state as High_BA or Lower_BA based on the median split
state_groups <- state_apps %>%
  mutate(group = if_else(total_apps >= median_apps, "High_BA", "Lower_BA"))

# Calculate each state's average monthly formation
state_form <- bfs_long %>%
  filter(series == "BF_SBF8Q") %>%
  group_by(geo) %>%
  summarise(mean_monthly_form = mean(count, na.rm = TRUE), .groups = "drop")

compare_df <- state_groups %>%
  inner_join(state_form, by = "geo")

# Welch Two Sample t-test to compare mean formations between High_BA and lower_ba states
t_test_result <- t.test(mean_monthly_form ~ group, data = compare_df)
print(t_test_result)

# p-value= 0.0002441
# The p-value is less than 0.05; therefore, REJECT the null hypothesis
# We compared mean monthly employer formations between states with high business application volumes and those with lower volumes
# High-application states have significantly higher average monthly employer business formations (about 1,227 formations per month) than lower-application states (about 220 per month)
# This shows a strong, statistically significant relationship between high business application activity and higher rates of employer formations.

# Show group sizes from t-test
compare_df %>%
  group_by(group) %>%
  summarise(n = n(), mean_form = mean(mean_monthly_form), sd_form = sd(mean_monthly_form))


### Additional Hypothesis Test
# Was there a post-pandemic drop? (2021–22 vs 2023–24)
# Null Hypothesis (H0): There is no difference in mean monthly total business applications 
#     between early (2021–2022) and later (2023–2024) post-pandemic periods.
# Alt. Hypothesis (H1): Mean monthly applications decreased in 2023–2024 compared to 2021–2022.

# Aggregate monthly totals across all states, tags each month as early (2021-22) or later (2023-24)
monthly_us_apps <- bfs_long %>%
  filter(series == "BA_BA") %>%
  group_by(date, year) %>%
  summarise(total_apps = sum(count, na.rm = TRUE), .groups = "drop") %>%
  mutate(period = if_else(year <= 2022, "EarlyPost_2021_2022", "LaterPost_2023_2024"))

# Boxplot to visualize the difference between early and later post-pandemic periods
ggplot(monthly_us_apps, aes(x = period, y = total_apps, fill = period)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Monthly Business Applications by Period",
    subtitle = "Comparing Early vs Later Post-Pandemic Years (2021–2024)",
    x = "Period",
    y = "Applications (All States)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Run an ANOVA test for differences between early and later periods
anova_period <- aov(total_apps ~ period, data = monthly_us_apps)
summary(anova_period)

# p-value = 0.679 (very high)
# Fail to reject the Null hypothesis
# There is no statistically significant difference in mean monthly business applications between the early post-pandemic period (2021–2022) and the later post-pandemic period (2023–2024).



### Consolidated & cleaned dataset
bfs_consolidated <- bfs_long %>%
  transmute(
    filing_date = date,
    year = as.integer(year),
    month = as.character(month),
    location = geo,
    filing_type = series,
    filing_type_label = series_label,
    formation_status = if_else(grepl("^BF_", series), "formation", "application"),
    count = as.integer(count)
  )


### Deliverable csv file
# Create a folder in the Rproj environment called "output" to store the cleaned dataset
dir.create("output", showWarnings = FALSE)
# Save the cleaned and consolidated bfs dataset as a new csv file in the output folder
write_csv(bfs_consolidated, "output/bfs_consolidated_2021_2024.csv")

#########################################
# End Script
#########################################