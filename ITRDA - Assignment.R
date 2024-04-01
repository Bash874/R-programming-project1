library(tidyverse)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

#reads the data file
data <- read.csv("C:/Users/Bhavesh/OneDrive/Documents/R studio Code/logbook.csv")
View(data)
glimpse(data)

tidy_data <- data
tidy_data$date_fueled = NULL
#tidy_data$user_url = NULL


#Views the dataset
View(tidy_data)

#Converts all columns from char to numeric
tidy_data$gallons <- as.numeric(as.character(tidy_data$gallons))
tidy_data$mpg <- as.numeric(as.character(tidy_data$mpg))
tidy_data$miles <- as.numeric(as.character(tidy_data$miles))
tidy_data$odometer <- as.numeric(gsub(",",".", tidy_data$odometer))

#View the data type
sapply(tidy_data, class)


# Remove currency symbols
tidy_data <- tidy_data %>%
  mutate(across(starts_with("cost_per_gallon") | starts_with("total_"), ~as.numeric(gsub("[^0-9.]", "", .))))

print(tidy_data)

# Check for duplicate values
duplicate_dates <- tidy_data$duplicate_dates[duplicated(tidy_data$data_captured) | duplicated(tidy_data$data_captured, fromLast = TRUE)]

# Prints the duplicate dates
print(duplicate_dates)


# Remove rows where only odometer is not NA but all other columns are N/A
tidy_data <- tidy_data[!(!is.na(tidy_data$odometer) & is.na(tidy_data$gallons) & is.na(tidy_data$cost_per_gallon) & is.na(tidy_data$total_spent) & is.na(tidy_data$mpg) & is.na(tidy_data$miles)), ]

# Check for missing values for each column
missing_values <- colSums(is.na(tidy_data))

# Prints the missing values
print(missing_values)

# Remove the 'miles' column from the original dataframe
tidy_data_without_miles <- subset(tidy_data, select = -c(miles))

# Create a new dataframe with just the 'miles' column
miles_data <- tidy_data[, "miles", drop = FALSE]


# Print the updated dataset
print(tidy_data)

summary(tidy_data)

summary(tidy_data_without_miles)

head(tidy_data)

View(tidy_data)

# Remove rows with missing values in the "odometer" column
tidy_data_without_miles <- tidy_data_without_miles[complete.cases(tidy_data_without_miles$odometer), ]

# Check the dimensions of the dataframe after removing rows
dim(tidy_data_without_miles)

# Check for missing values for each column
New_missing_values <- colSums(is.na(tidy_data_without_miles))

# Prints the missing values
print(New_missing_values)

View(tidy_data_without_miles)

# Summary stats 
summary(tidy_data_without_miles$gallons)


# Count missing values for the "miles" column
missing_miles <- sum(is.na(miles_data$miles))

# Prints the number of missing values
print(missing_miles)


# Filters rows with missing values in specified columns
rows_missing <- tidy_data_without_miles[is.na(tidy_data_without_miles$cost_per_gallon) | is.na(tidy_data_without_miles$total_spent), ]

# Displays the rows with missing values
print(rows_missing)

# Convert "date_captured" column to date 
tidy_data_without_miles$date_captured <- as.Date(tidy_data_without_miles$date_captured, format = "%b %d %Y")

# Check the structure of the dataframes
str(tidy_data_without_miles)
View(tidy_data_without_miles)


# Plots the distributions for each of the columns mentioned below 
ggplot(tidy_data_without_miles, aes(x = date_captured)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Fuelling Dates", y = "Frequency") +
  ggtitle("Fuelling Dates")

# Histogram of mpg
ggplot(tidy_data_without_miles, aes(x = mpg)) +
  geom_histogram(binwidth = 15, fill = "lightgreen", color = "black") +
  labs(x = "Miles per Gallon (mpg)", y = "Frequency") +
  ggtitle("Miles per Gallon")

# Histogram of miles
ggplot(miles_data, aes(x = miles)) +
  geom_histogram(binwidth = 50, fill = "lightpink", color = "black") +
  labs(x = "Miles", y = "Frequency") +
  ggtitle("Miles Driven")

# Histogram of gallons
ggplot(tidy_data_without_miles, aes(x = gallons)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Gallons", y = "Frequency") +
  ggtitle("Gallons Used")


#Question 2 - Five New Features


#1 -Fuel Cost per Month 


# Calculates fuel cost per month
tidy_data_without_miles$fuel_cost_pm <- round((tidy_data_without_miles$gallons * tidy_data_without_miles$cost_per_gallon) /
                                                       (as.numeric(difftime(tail(tidy_data_without_miles$date_captured, 1),
                                                                            head(tidy_data_without_miles$date_captured, 1),
                                                                            units = "days")) / 30), 2)

# View the updated dataframe
head(tidy_data_without_miles)


#2 Avg MPG

# Calculate average MPG by year
avg_mpg_by_year <- tidy_data_without_miles %>%
  group_by(year(date_captured)) %>%
  summarise(Avg_MPG = mean(mpg, na.rm = TRUE))

# View the result
print(avg_mpg_by_year)


View(tidy_data_without_miles)



#3 Fuel Efficiency Rating

# Sample data
Fuel_Rating <- data.frame(
  Year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  avg_MPG = c(19.6, 19.7, 20.0, 19.9, 20.9, 21.8, 21.8, 22.3, 21.7, 21.1, 21.0, 21.4, 21.7, 22.1, 21.7)
)

# Categorize fuel efficiency rating
Fuel_Rating$fuel_efficiency <- ifelse(Fuel_Rating$avg_MPG < 20, "Low",
                                             ifelse(Fuel_Rating$avg_MPG >= 20 & Fuel_Rating$avg_MPG <= 25, "Medium", "High"))

# View the categorized data
print(Fuel_Rating)


#4 Price Increase

ggplot(Fuel_Rating, aes(x = Year, y = avg_MPG, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Average MPG", title = "Price Increase Trend Over The Years")


View(tidy_data_without_miles)

#5 

# Calculate earliest and latest dates
earliest_date <- min(tidy_data_without_miles$date_captured)
latest_date <- max(tidy_data_without_miles$date_captured)

# Calculate range of dates
date_range <- range(tidy_data_without_miles$date_captured)

# Count number of fill-ups per month
fillups_per_month <- table(format(tidy_data_without_miles$date_captured, "%Y-%m"))

# Calculate fill-ups per month
fillups_per_month <- as.data.frame(table(format(tidy_data_without_miles$date_captured, "%Y-%m")))
colnames(fillups_per_month) <- c("Year_Month", "Fillups_Count")

# View the updated dataframe
View(fillups_per_month)


#Question 2.2 

# Create a new dataframe with only the 'total_spent' column
currency_values <- data.frame(total_spent = data$total_spent)

# Replace whitespace with NA in the specified column of currency_values
currency_values$total_spent <- gsub("^\\s*$", NA, currency_values$total_spent)

# Remove rows with NA values
currency_values <- na.omit(currency_values)

# View the updated dataframe
View(currency_values)


# Extract currency symbols using gsub
currency_symbols <- gsub("[0-9.]", "", currency_values$total_spent)

# Count occurrences of each currency symbol
currency_counts <- table(currency_symbols)

# Sort the counts in descending order and select the top 5 currencies
sorted_currency_counts <- sort(currency_counts, decreasing = TRUE)
top_currencies <- names(sorted_currency_counts)[1:5]

# Filter the counts for the top 5 currencies
top_currency_counts <- sorted_currency_counts[top_currencies]

# Display the top 5 currencies and their counts
top_currency_counts


#Question 3

# Extract car make and model from user_url
tidy_data_without_miles$car_make_model <- gsub(".*/([^/]+)/([^/]+)/.*", "\\1 \\2", tidy_data_without_miles$user_url)

# Remove the user_url column
tidy_data_without_miles <- subset(tidy_data_without_miles, select = -user_url)

# View the updated dataframe
head(tidy_data_without_miles)


# Create a dataframe for the top 5 currencies and their corresponding counts
Top5_Currencies <- data.frame(
  Currency = c("$", "£", "€", "CA$", "R"),
  Count = c(649318, 77906, 52968, 41767, 26522)
)


# Create insight_data dataframe with 'total_spent' and 'user_url' columns
insight_data <- data.frame(total_spent = data$total_spent, user_url = data$user_url)


# Replace whitespaces with NA in the 'user_url' column of insight_data
insight_data$total_spent <- gsub("^\\s*$", NA, insight_data$total_spent)

# Remove rows with NA values
insight_data <- na.omit(insight_data)

# Check the updated dataframe
head(insight_data)
insight_data$car_model <- gsub(".*/([^/]+)/([^/]+)/.*", "\\1 \\2", insight_data$user_url)
insight_data$cars <- insight_data$car_model
insight_data$user_url <- NULL  # Remove the original user_url column
insight_data$cars <- NULL

#Prints the first 5 rows
head(insight_data)

insight_data$currency <- gsub("[0-9.]", "", insight_data$total_spent)

# Counts each of the each currency symbol
currency_counts <- table(insight_data$currency)
top_currencies <- names(sort(currency_counts, decreasing = TRUE)[1:5])

# Filter 'insight_data' based on top 5 currencies
filtered_data <- insight_data[insight_data$currency %in% top_currencies, ]

# Group filtered data by 'car_model'
popular_cars <- filtered_data %>%
  group_by(car_model) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5)  # Select the most popular car for each currency

# Display the top 5 currencies and their most popular cars
popular_cars
print(top_currencies)


# Filter insight_data for the top 5 currencies
filtered_data <- insight_data[insight_data$currency %in% top_currencies, ]

# Count each car make and model for the top 5 currencies
car_counts <- table(filtered_data$car_model)

# Sort the counts in descending order
sorted_car_counts <- sort(car_counts, decreasing = TRUE)

top_10_cars <- head(names(sorted_car_counts), 10)

# Create a dataframe with the top 10 most popular cars and their counts
top_cars_df <- data.frame(car_make_model = top_10_cars, count = sorted_car_counts[1:10])


# Filter the data for the top 5 currencies
top_currencies <- c("$", "£", "€", "CA$", "R")
filtered_data <- insight_data[insight_data$currency %in% top_currencies, ]

# Count occurrences of each car model for the top 5 currencies
car_counts <- table(filtered_data$car_model, filtered_data$currency)

# Initialize a list to store the most popular car model for each currency
most_popular <- list()

# Loop through each currency to find the most popular car model
for (currency in colnames(car_counts)) {
  counts <- car_counts[, currency]
  if (sum(counts) > 0) {
    max_count <- max(counts)
    max_models <- names(counts[counts == max_count])
    most_popular[[currency]] <- data.frame(currency = currency, car_model = max_models, count = max_count)
  }
}

# Combines the results into a single dataframe
most_popular_df <- do.call(rbind, most_popular)

# Displays the most popular car model for each of the top currency
most_popular_df

# Data
plot_data <- data.frame(
  currency = c("£", "€", "CA$", "R"),
  car_model = c("£ 335d_xdrive 2015", "€ giulietta 2014", "CA$ golf_city 2008", "R jimny 2010"),
  count = c(158, 146, 120, 125)
)
# Bar plot
ggplot(plot_data, aes(x = currency, y = count, fill = car_model)) +
  geom_bar(stat = "identity") +
  labs(x = "Currency", y = "Count", fill = "Car Model") +
  ggtitle("Count of Car Models by Currency") +
  theme_minimal()

#question 3.1 b...
library(dplyr)
library(ggplot2)

# Rename column in insight_data
insight_data <- insight_data %>%
  rename(car_make_model = car_model)

# Filter and mutate tidy_data_without_miles in place
tidy_data_without_miles <- tidy_data_without_miles %>%
  filter(format(date_captured, "%Y-%m") == "2022-01")

# Create jan_2022 dataframe directly without specifying individual obs
jan_2022 <- data.frame(
  Year_Month = "2022-01",
  Fillups_Count = 39530,
  total_spent = c(2000, 3000, 2500),  # Example values for total spent in USD
  total_litres = c(500, 700, 600)  # Example values for total litres consumed
)

# Calculate cost per litre directly in the jan_2022 dataframe
jan_2022 <- jan_2022 %>%
  mutate(cost_per_litre = total_spent / total_litres)

# Left join insight_data with tidy_data_without_miles without creating intermediate dataframes
insight_data <- insight_data %>%
  left_join(tidy_data_without_miles %>% select(car_make_model, odometer), suffix = c(".insight", ".tidy"))



# Select unique values from each column
unique_data <- insight_data %>%
  distinct(total_spent, car_make_model, currency, odometer, .keep_all = TRUE)

# Calculate the average distance for each country
average_distance <- unique_data %>%
  group_by(currency) %>%
  summarise(avg_distance = mean(odometer, na.rm = TRUE))

# Find the country with the highest average distance
highest_avg_distance <- average_distance %>%
  top_n(1, avg_distance)

# Print the country with the highest average distance
print(highest_avg_distance)

# Find the row with the highest average distance
highest_avg_distance <- average_distance[which.max(average_distance$avg_distance), ]

#3c
# Plotting only the highest average distance by country using ggplot with a bar graph
ggplot() +
  geom_point(data = highest_avg_distance, aes(x = currency, y = avg_distance), size = 4, color = "darkblue") +
  labs(title = "Highest Average Distance by Country", x = "Currency", y = "Average Distance") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  



#3d

ggplot(Fuel_Rating, aes(x = fuel_efficiency, y = avg_MPG, color = Year)) +
  geom_point() +  # Add points for each data point
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without confidence intervals
  labs(title = "Relationship Between Fuel Efficiency and Average MPG by Year",
       x = "Fuel Efficiency",
       y = "Average MPG",
       color = "Year") +
  theme_minimal()


#3e
fuel_data <- unique_data %>%
  select(car_make_model, currency) %>%
  filter(currency == "R")

# View the filtered data
View(fuel_data)

car_counts <- fuel_data %>%
  count(car_make_model)

# Select the top 5 most popular cars
SAtop_cars <- car_counts %>%
  top_n(5, n)

# View the top 5 most popular cars
head(SAtop_cars)


# dataset for 3e
SAdata <- data.frame(
  Year = c(2011, 2011, 2011, 2012, 2018),
  Fuel = c("Low", "Low", "Low", "Medium", "Medium"),
  SA_car = c("fortuner", "hilux", "jimmy", "a4", "ranger")
)

# View the dataset
print(SAdata)

ggplot(SAdata, aes(x = SA_car, fill = Fuel)) +
  geom_bar() +
  labs(title = "5 most popular vehicles in SA", x = "SA Cars") +
  facet_wrap(~ Year) +
  theme_minimal()


#3f
fuel_efficiency_data <- tidy_data_without_miles %>%
  select(odometer , gallons, cost_per_gallon, total_spent, mpg, fuel_cost_pm)

# Calculate the correlation matrix
correlation_matrix <- cor(fuel_efficiency_data)

# Find the top three features that fuel efficiency is most strongly correlated with
top_correlated_features <- colnames(fuel_efficiency_data)[order(-correlation_matrix[,"mpg"])][2:4]

# Print the top correlated features
print(correlation_matrix)

#q4
library(stringr)
library(lubridate)

#selects only the first index of the the char
sa_users <- data[str_sub(data$cost_per_gallon, 1, 1) == "R", ]

# Views the new dataframe
head(sa_users$cost_per_gallon)

sa_usersR <- sa_users %>%
  filter(grepl("^R\\d+\\.\\d+$", cost_per_gallon))

# View the filtered dataframe
head(sa_usersR)

#4a

sa_usersR_fuel <- sa_usersR[, c("date_captured", "cost_per_gallon")]
View(sa_usersR_fuel)

sa_usersR_fuel <- sa_usersR_fuel %>%
  mutate(date_captured = as.Date(date_captured, format = "%b %d %Y"))

sa_usersR_fuel <- sa_usersR_fuel %>%
  mutate(month = month(date_captured),
         year = year(date_captured))

#Views the first 5 rows
head(sa_usersR_fuel)

sa_usersR_fuel$cost_per_gallon_cleaned <- as.numeric(gsub("[^0-9.]", "", sa_usersR_fuel$cost_per_gallon))

# Plot the scatter plot with aggregated data

ggplot(sa_usersR_fuel, aes(x = year, y = cost_per_gallon_cleaned)) +
  geom_point() +  # Scatter plot
  labs(title = "Change in Fuel Prices Over Time",
       x = "Year",
       y = "Cost per Gallon") +
  theme_minimal()


#4b
sa_usersR_fuel$day_of_week <- wday(sa_usersR_fuel$date_captured, label = TRUE)

day_colours <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a")

# Plotting the number of users filling up each day of the week with different colors for each day
ggplot(sa_usersR_fuel, aes(x = day_of_week, fill = day_of_week)) +
  geom_bar() +
  labs(title = "Trends of Users Filling Up Each Day of the Week in SA",
       x = "Day of the Week",
       y = "Number of Fill ups") +
  scale_fill_manual(values = day_colours) +
  theme_minimal()

#4c
day_counts <- sa_usersR_fuel %>%
  count(day_of_week)
print(day_counts)

average_other_days <- mean(day_counts$n[day_counts$day_of_week != "Tue"])

# Calculate the number of refueling occurrences on Tuesday
refuel_tuesday <- day_counts$n[day_counts$day_of_week == "Tue"]

# Calculate the difference
difference <- round(refuel_tuesday - average_other_days)
# Round the difference to the nearest whole number


cat("Difference in the amount of people refueling on a Tuesday:", difference)

# Create a bar plot to show the difference in refueling occurrences on Tuesday vs other days
ggplot(day_counts, aes(x = day_of_week, y = n, fill = ifelse(day_of_week == "Tue", "Tuesday", "Other Days"))) +
  geom_bar(stat = "identity") +
  labs(title = "Difference in Refueling Occurrences on Tuesday vs Other Days",
       x = "Day of the Week",
       y = "Number of Refueling Occurrences",
       fill = "Day Type") +
  theme_minimal()+
  geom_text(aes(label = ifelse(day_of_week == "Tue", paste("Difference=:", difference), "")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Tuesday" = "red", "Other Days" = "lightgray")) 

#4d


# Filter data for the year 2020
sa_usersR_2020 <- sa_usersR_fuel %>%
  filter(year == 2020) %>%
  select(date_captured, cost_per_gallon_cleaned)

# View the filtered data
head(sa_usersR_2020)


sa_usersR_2020$cluster <- ifelse(sa_usersR_2020$cost_per_gallon_cleaned > 50, "High", "Low")

# Plotting the scatter plot with colored clusters based on the 'cluster' variable
ggplot(sa_usersR_2020, aes(x = date_captured, y = cost_per_gallon_cleaned, color = cluster)) +
  geom_point() +  # Scatter plot
  labs(title = "year 2020,Fuel price",
       x = "Date",
       y = "Cost per Gallon",
       color = "Fuel Price") +  # Specify the legend title for the color scale
  theme_minimal()



