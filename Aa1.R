# Set the working directory and verify it
setwd("E:\\VCU\\Summer 2024\\Statistical Analysis & Modeling")
getwd()

# Install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Read the file into R
data <- read.csv("NSSO68.csv")

# Filter for Maharashtra
mh_df <- data %>% filter(state_1 == "MH")

# Display dataset info
cat("Dataset Information:\n")
print(names(mh_df))
print(head(mh_df))
print(dim(mh_df))

# Finding missing values
missing_info <- colSums(is.na(mh_df))
cat("Missing Values Information:\n")
print(missing_info)

any(is.na(mh_df))
sum(is.na(mh_df))

# Sub-setting the data
mhnew <- mh_df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(mhnew)))

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
mhnew$Meals_At_Home <- impute_with_mean(mhnew$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(mhnew)))

# Find outliers and removing them
boxplot(mhnew$ricepds_v)
remove_outliers <- function(mh_df, column_name) {
  Q1 <- quantile(mh_df[[column_name]], 0.25)
  Q3 <- quantile(mh_df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  mh_df <- subset(mh_df, mh_df[[column_name]] >= lower_threshold & mh_df[[column_name]] <= upper_threshold)
  return(mh_df)
}


outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  mhnew <- remove_outliers(mhnew, col)
}

# Renaming districts and sectors
district_mapping <- c( "1" = "Nandurbar", "2" = "Dhule", "3" = "Jalgaon", "4" = "Buldana", "5" = "Akola", "6" = "Washim", "7" = "Amravati", "8" = "Wardha", "9" = "Nagpur", "10" = "Bhandara", "11" = "Gondiya", "12" = "Gadchiroli", "13" = "Chandrapur", "14" = "Yavatmal", "15" = "Nanded", "16" = "Hingoli", "17" = "Parbhani", "18" = "Jalna", "19" = "Aurangabad", "20" = "Nashik", "21" = "Thane", "22" = "Mumbai", "24" = "Raigarh", "25" = "Pune", "26" = "Ahmadnagar", "27" = "Bid", "28" = "Latur", "29" = "Osmanabad", "30" = "Solapur", "31" = "Satara", "32" = "Ratnagiri", "33" = "Sindhudurg", "34" = "Kolhapur", "35" = "Sangli" )
sector_mapping <- c("1" = "Rural", "2" = "Urban")

mhnew$District <- as.character(mhnew$District)
mhnew$Sector <- as.character(mhnew$Sector)
mhnew$District <- ifelse(mhnew$District %in% names(district_mapping), district_mapping[mhnew$District], mhnew$District)
mhnew$Sector <- ifelse(mhnew$Sector %in% names(sector_mapping), sector_mapping[mhnew$Sector], mhnew$Sector)

# Summarize consumption
mhnew$total_consumption <- rowSums(mhnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)


# Test for differences in mean consumption between urban and rural
rural <- mhnew %>%
  filter(Sector == "Rural") %>%
  select(total_consumption)

urban <- mhnew %>%
  filter(Sector == "Urban") %>%
  select(total_consumption)

mean_rural <- mean(rural$total_consumption)
mean_urban <- mean(urban$total_consumption)

# Perform z-test
z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

# Generate output based on p-value
if (z_test_result$p.value < 0.05) {
  cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
  cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
} else {
  cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
  cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
}
