library(ggplot2)
library(dplyr)

airbnb_data <- read.csv("Airbnb_Data_ChrisDraper.csv")
head(airbnb_data)  
str(airbnb_data)   

airbnb_data$first_review <- as.Date(airbnb_data$first_review, format = "%m/%d/%Y")
airbnb_data$last_review <- as.Date(airbnb_data$last_review, format = "%m/%d/%Y")
airbnb_data$host_since <- as.Date(airbnb_data$host_since, format = "%m/%d/%Y")

airbnb_data$host_has_profile_pic <- ifelse(airbnb_data$host_has_profile_pic == "t", 1, 0)

table(airbnb_data$host_has_profile_pic)

airbnb_data$host_identity_verified <- ifelse(airbnb_data$host_identity_verified == "t", 1, 0)

table(airbnb_data$host_identity_verified)  

airbnb_data$cleaning_fee_numeric <- as.numeric(airbnb_data$cleaning_fee)

table(airbnb_data$cleaning_fee_numeric)  

airbnb_data$instant_bookable <- ifelse(airbnb_data$instant_bookable == "t", TRUE, FALSE)
airbnb_data$instant_bookable_numeric <- as.numeric(airbnb_data$instant_bookable)
table(airbnb_data$instant_bookable_numeric)

airbnb_data$X <- NULL

airbnb_data$host_response_rate <- as.numeric(gsub("%", "", airbnb_data$host_response_rate))

summary(airbnb_data)

airbnb_data$property_type <- factor(airbnb_data$property_type)

str(airbnb_data$property_type)

airbnb_data$room_type <- factor(airbnb_data$room_type)

str(airbnb_data$room_type)
levels(airbnb_data$room_type)

airbnb_data$bed_type <- factor(airbnb_data$bed_type)
str(airbnb_data$bed_type)
levels(airbnb_data$bed_type)


levels(airbnb_data$cancellation_policy)[levels(airbnb_data$cancellation_policy) == "super_strict_30"] <- "super_strict"
levels(airbnb_data$cancellation_policy)[levels(airbnb_data$cancellation_policy) == "super_strict_60"] <- "super_strict"
table(airbnb_data$cancellation_policy)

airbnb_data$cancellation_policy <- factor(airbnb_data$cancellation_policy)
str(airbnb_data$cancellation_policy)
levels(airbnb_data$cancellation_policy)

airbnb_data$city <- factor(airbnb_data$city)
str(airbnb_data$city)
levels(airbnb_data$city)

property_type_counts <- table(airbnb_data$property_type)
property_type_counts <- sort(property_type_counts, decreasing = TRUE) 

top_property_types <- names(property_type_counts[1:5])
print(top_property_types)

###########################################################

chicago_data <- airbnb_data[airbnb_data$city == "Chicago", ]

str(chicago_data)

summary(chicago_data)

#######################

remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  filtered_data <- df[df[[column_name]] >= lower_bound & df[[column_name]] <= upper_bound, ]
  return(filtered_data)
}

chicago_data_filtered <- remove_outliers(chicago_data, "log_price")
chicago_data_filtered <- remove_outliers(chicago_data_filtered, "host_response_rate")
chicago_data_filtered <- remove_outliers(chicago_data_filtered, "number_of_reviews")
chicago_data_filtered <- remove_outliers(chicago_data_filtered, "review_scores_rating")

summary(chicago_data_filtered[c("log_price", "host_response_rate", "number_of_reviews", "review_scores_rating")])
cat("Number of observations after removing outliers:", nrow(chicago_data_filtered), "\n")

###################################################################

chicago_data_filtered$combined_score <- log1p(chicago_data_filtered$number_of_reviews) * chicago_data_filtered$review_scores_rating

####################################################################
chicago_data_filtered <- chicago_data_filtered[chicago_data_filtered$property_type %in% top_property_types, ]

chicago_data_filtered$property_type <- factor(chicago_data_filtered$property_type)

levels(chicago_data_filtered$property_type)
########################################################################


ggplot(chicago_data_filtered, aes(x = property_type, y = combined_score, fill = property_type)) +
  geom_boxplot() +
  labs(title = "Box Plot of Combined Scores by Property Type in Chicago",
       x = "Property Type",
       y = "Combined Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 


#####################################################################


ggplot(chicago_data_filtered, aes(x = room_type, y = combined_score, fill = room_type)) +
  geom_boxplot() +
  labs(title = "Box Plot of Combined Scores by Room Type in Chicago",
       x = "Room Type",
       y = "Combined Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

####################################################################

room_property_table <- table(chicago_data_filtered$room_type, chicago_data_filtered$property_type)

print(room_property_table)

ggplot(data = as.data.frame(room_property_table), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Room Types by Property Type",
       x = "Property Type",
       y = "Count",
       fill = "Room Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################################################

ggplot(chicago_data_filtered, aes(x = combined_score, y = log_price)) +
  geom_point(aes(color = room_type), alpha = 0.5) +
  labs(title = "Scatter Plot of Log Price by Combined Score",
       x = "Combined Score",
       y = "Log Price",
       color = "Room Type") +
  theme_minimal() +
  theme(legend.position = "right")

###################################################################

filtered_data <- chicago_data_filtered %>%
  filter(room_type == "Entire home/apt", 
         property_type %in% c("Apartment", "Condominium", "House"))

mean_scores_by_accommodates <- filtered_data %>%
  group_by(accommodates) %>%
  summarise(mean_combined_score = mean(combined_score, na.rm = TRUE))

ggplot(mean_scores_by_accommodates, aes(x = factor(accommodates), y = mean_combined_score, fill = factor(accommodates))) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Combined Score by Accommodates for Entire Homes/Apts",
       subtitle = "Filtered by Property Types: Apartment, Condominium, House",
       x = "Accommodates",
       y = "Mean Combined Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

####################################################################


filtered_data <- chicago_data_filtered %>%
  filter(room_type == "Entire home/apt", 
         property_type %in% c("Apartment", "Condominium", "House"))

ggplot(filtered_data, aes(x = combined_score, y = accommodates)) +
  geom_point(aes(color = property_type), alpha = 0.6) +
  labs(title = "Scatter Plot of Combined Score vs. Accommodates",
       subtitle = "Filtered by Property Types: Apartment, Condominium, House",
       x = "Combined Score",
       y = "Accommodates") +
  scale_color_manual(values = c("Apartment" = "blue", "Condominium" = "green", "House" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

#######################################################################

filtered_data <- chicago_data_filtered %>%
  filter(room_type == "Entire home/apt", 
         property_type %in% c("Apartment", "Condominium", "House"))

ggplot(filtered_data, aes(x = log_price, y = accommodates)) +
  geom_point(aes(color = property_type), alpha = 0.6) +
  labs(title = "Scatter Plot of log_price vs. Accommodates",
       subtitle = "Filtered by Property Types: Apartment, Condominium, House",
       x = "log_price",
       y = "Accommodates") +
  scale_color_manual(values = c("Apartment" = "blue", "Condominium" = "green", "House" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

########################################################################

filtered_data <- chicago_data_filtered %>%
  filter(room_type == "Entire home/apt", 
         property_type %in% c("Apartment", "Condominium", "House"))

ggplot(filtered_data, aes(x = log_price, y = accommodates)) +
  geom_point(aes(color = property_type), alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(title = "Scatter Plot of log_price vs. Accommodates",
       subtitle = "Filtered by Property Types: Apartment, Condominium, House",
       x = "log_price",
       y = "Accommodates") +
  scale_color_manual(values = c("Apartment" = "blue", "Condominium" = "green", "House" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

##############################

top_zipcodes <- filtered_data %>%
  group_by(zipcode) %>%
  summarise(Count = n()) %>%
  top_n(10, Count) %>%
  pull(zipcode)

filtered_data_top_zipcodes <- filtered_data %>%
  filter(zipcode %in% top_zipcodes)

filtered_data_top_zipcodes$zipcode <- factor(filtered_data_top_zipcodes$zipcode, levels = top_zipcodes)

ggplot(filtered_data_top_zipcodes, aes(x = zipcode, y = combined_score)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Box Plot of Combined Score by Zipcode",
       x = "Zipcode",
       y = "Combined Score")

###############################

filtered_data <- filtered_data %>% 
  filter(!is.na(bathrooms) & !is.na(combined_score))

ggplot(filtered_data, aes(x = bathrooms, y = combined_score)) +
  geom_point(alpha = 0.6, color = "blue") + 
  theme_minimal() +
  labs(title = "Scatter Plot of Combined Score vs. Bathrooms",
       x = "Number of Bathrooms",
       y = "Combined Score")

#####

filtered_data <- filtered_data %>%
  filter(!is.na(bedrooms) & !is.na(combined_score))

ggplot(filtered_data, aes(x = bedrooms, y = combined_score)) +
  geom_point(alpha = 0.6, color = "green") +
  theme_minimal() +
  labs(title = "Scatter Plot of Combined Score vs. Bedrooms",
       x = "Number of Bedrooms",
       y = "Combined Score")

#############

library(dplyr)
library(ggplot2)

if(all(c("log_price", "property_type", "accommodates", "bathrooms", "bed_type", "bedrooms", "combined_score") %in% names(filtered_data))) {
  
  filtered_data$property_type <- as.factor(filtered_data$property_type)
  filtered_data$bed_type <- as.factor(filtered_data$bed_type)
  
  model <- lm(combined_score ~ log_price + property_type + accommodates + bathrooms + bed_type + bedrooms, 
              data = filtered_data)
  
  summary_model <- summary(model)
  print(summary_model)
} else {
  cat("Error: Not all required variables are present in the dataset.\n")
}















