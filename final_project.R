library(readxl)
library(dplyr)
library(ggplot2)

# Set the file paths to the actual locations of your Excel files
path_to_inventory <- "data/HOSINVUSM495N.xls"
path_to_median_days <- "data/MEDDAYONMARUS.xls"
path_to_price_index <- "data/USSTHPI.xls"

# Load the datasets
housing_inventory <- read_excel(path_to_inventory)
median_days_on_market <- read_excel(path_to_median_days)
housing_price_index <- read_excel(path_to_price_index)

# Check column names to ensure they are correct
print(colnames(housing_inventory))
print(colnames(median_days_on_market))
print(colnames(housing_price_index))

# Data Cleaning 
housing_inventory <- housing_inventory %>%
  mutate(
    observation_date = as.Date(observation_date, format = "%Y-%m-%d"),
    Housing_Inventory_2023 = as.numeric(Housing_Inventory_2023)
  )

# Data Cleaning
median_days_on_market <- median_days_on_market %>%
  mutate(
    observation_date = as.Date(observation_date, format = "%Y-%m-%d"),
    MED_DAY_ON_MARUS = as.numeric(MED_DAY_ON_MARUS)
  )

# Data Cleaning
housing_price_index <- housing_price_index %>%
  mutate(
    observation_date = as.Date(observation_date, format = "%Y-%m-%d"),
    USSTHPI = as.numeric(USSTHPI)
  )

# EDA for Housing Inventory
housing_inventory_eda <- housing_inventory %>%
  summarise(
    Min_Inventory = min(Housing_Inventory_2023, na.rm = TRUE),
    Max_Inventory = max(Housing_Inventory_2023, na.rm = TRUE),
    Mean_Inventory = mean(Housing_Inventory_2023, na.rm = TRUE),
    SD_Inventory = sd(Housing_Inventory_2023, na.rm = TRUE)
  )

# EDA for Median Days on Market
median_days_eda <- median_days_on_market %>%
  summarise(
    Min_Days = min(MED_DAY_ON_MARUS, na.rm = TRUE),
    Max_Days = max(MED_DAY_ON_MARUS, na.rm = TRUE),
    Mean_Days = mean(MED_DAY_ON_MARUS, na.rm = TRUE),
    SD_Days = sd(MED_DAY_ON_MARUS, na.rm = TRUE)
  )

# EDA for Housing Price Index
price_index_eda <- housing_price_index %>%
  summarise(
    Min_Index = min(USSTHPI, na.rm = TRUE),
    Max_Index = max(USSTHPI, na.rm = TRUE),
    Mean_Index = mean(USSTHPI, na.rm = TRUE),
    SD_Index = sd(USSTHPI, na.rm = TRUE)
  )

# Plotting Housing Inventory
ggplot(housing_inventory, aes(x = observation_date, y = Housing_Inventory_2023)) +
  geom_line() +
  labs(title = "Housing Inventory Over Time",
       x = "Date", y = "Inventory (Units)") +
  theme_minimal()

# Plotting Median Days on Market
ggplot(median_days_on_market, aes(x = observation_date, y = MED_DAY_ON_MARUS)) +
  geom_line(color = "blue") +
  labs(title = "Median Days on Market Over Time",
       x = "Date", y = "Median Days on Market") +
  theme_minimal()

# Plotting Housing Price Index
ggplot(housing_price_index, aes(x = observation_date, y = USSTHPI)) +
  geom_line(color = "green") +
  labs(title = "Housing Price Index Over Time",
       x = "Date", y = "Price Index") +
  theme_minimal()

# Display the EDA summaries
print(housing_inventory_eda)
print(median_days_eda)
print(price_index_eda)

