setwd("C:/Users/Chris/OneDrive/Desktop/archive")
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

library(ggplot2)

ggplot(chicago_data_filtered, aes(x = property_type, y = combined_score, fill = property_type)) +
  geom_boxplot() +
  labs(title = "Box Plot of Combined Scores by Property Type in Chicago",
       x = "Property Type",
       y = "Combined Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") 


#####################################################################

library(ggplot2)

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

library(ggplot2)
ggplot(data = as.data.frame(room_property_table), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Room Types by Property Type",
       x = "Property Type",
       y = "Count",
       fill = "Room Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
















