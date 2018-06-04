reviews <- read.csv("yelp_academic_dataset_review.csv")

businesses <- read.csv("yelp_academic_dataset_business.csv")

users <- read.csv("yelp_academic_dataset_user.csv")

library(dplyr)
# Combine the reviews and users data sets
ru  <- inner_join(reviews, users)

# combine the newly created data set with the businesses data set
rub <- inner_join(ru, businesses)

# Take a look at the combined data frame
summary(rub)


# Explore the `reviews` data eset with `summary()` 
summary(reviews)

# Explore the `users` data set with `summary()` 
summary(users)

# Explore the `businesses` data set with `summary()` 
summary(businesses)

# Make dplry package avaiable to use
library(dplyr)

# Combine the reviews and users data sets
ru  <- inner_join(reviews, users)

# combine the newly created data set with the businesses data set
rub <- inner_join(ru, businesses)

# Take a look at the combined data frame
summary(rub)

# Create indian review column
rub$is_indian <- grepl("Indian", rub$categories) == TRUE

# Select only reviews for Indian restaurants
indian <- subset(rub, is_indian == TRUE)

### 6
# The package dplyr is available to use
# Generate a new data frame with the number of reviews by each reviewer
## Tries to find how many times a user_id exists...creates a table in a new coulumn 
## total_reviews....meaning trying to find number of times
## a reviewer has given a review
## one reviewer was found to have given 21 reviews

number_reviews_indian <- indian %>% 
  select(user_id, user_name) %>%
  group_by(user_id) %>% 
  summarise(total_reviews = n())

# Print the table of total_reviews
table(number_reviews_indian$total_reviews)

# Pring the average number of reviews per users
mean(number_reviews_indian$total_reviews)

# The package dplyr is available to use
# Combine number of Indian reviews with original data frame of Indian restaurant reviews
indian_plus_number <- inner_join(indian,number_reviews_indian)

# Display column names for the new data frame
names(indian_plus_number)

###7
