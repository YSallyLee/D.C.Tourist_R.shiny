library(tidyverse)
library(caret)

# Read the raw data of Airbnb in Washington, D.C. on 21 June, 2024
# from "Inside Airbnb" ("https://insideairbnb.com/get-the-data/").
raw_data <- read.csv("./data-raw/Airbnb_DC_raw_June2024.csv")

# Clean the data
cleaned_data <- raw_data |>
  # Select and reorder columns
  select(
    name,
    neighbourhood_cleansed,
    price,
    latitude,
    longitude,
    property_type,
    accommodates,
    listing_url,
    bathrooms,
    bedrooms,
    beds,
    license,
    host_since,
    host_response_time,
    host_response_rate,
    host_acceptance_rate,
    review_scores_rating,
    review_scores_accuracy,
    review_scores_cleanliness,
    review_scores_checkin,
    review_scores_communication,
    review_scores_location,
    review_scores_value,
    reviews_per_month
  ) |>
  # Remove '$' symbol and convert the column to numeric (dbl)
  mutate(
    price = str_replace_all(price, "\\$", ""),
    price = ifelse(price == "", NA, price),
    price_USD = as.numeric(price),
    .after = price
  ) |>
  select(-price) |>
  # Remove rows where 'price_USD' is NA
  filter(!is.na(price_USD)) |>
  # Group the property_type column
  mutate(
    property_group = case_when(
      property_type %in% c(
        "Casa particular", "Entire bungalow", "Entire condo",
        "Entire guest suite", "Entire guesthouse", "Entire home",
        "Entire rental unit", "Entire serviced apartment", "Entire townhouse",
        "Entire vacation home", "Tiny home"
      ) ~ "Entire Properties",
      property_type %in% c(
        "Entire loft", "Floor"
      ) ~ "Entire Floor/Loft",
      property_type %in% c(
        "Private room in bungalow", "Private room in casa particular",
        "Private room in condo", "Private room in guest suite",
        "Private room in guesthouse", "Private room in home",
        "Private room in rental unit", "Private room in resort",
        "Private room in townhouse", "Private room in villa",
        "Room in aparthotel", "Room in boutique hotel",
        "Room in hotel", "Room in serviced apartment"
      ) ~ "Private room",
      property_type %in% c(
        "Room in bed and breakfast", "Private room in bed and breakfast"
      ) ~ "Private room and breakfast",
      property_type %in% c(
        "Room in hostel", "Shared room in guesthouse", "Shared room in home",
        "Shared room in hostel", "Shared room in hotel",
        "Shared room in rental unit", "Shared room in townhouse"
      ) ~ "Shared room"
    ),
    .after = property_type
  ) |>
  select(-property_type) |>
  # Replace bathrooms values of 0 or blank with 1
  # After checking, bathrooms with values of 0 or blank from property_type have
  # at least one bathroom, so they are all changed to 1.
  mutate(bathrooms = ifelse(is.na(bathrooms) | bathrooms == 0, 1, bathrooms)) |>
  # Replace blank or 0 in 'bedrooms' with 'beds' values
  mutate(
    bedrooms = ifelse(bedrooms == 0 | is.na(bedrooms), beds, bedrooms)
  ) |>
  # Drop the 'beds' column
  select(-beds) |>
  # Replace remaining blank or 0 in 'bedrooms' with 1
  mutate(
    bedrooms = ifelse(bedrooms == 0 | is.na(bedrooms), 1, bedrooms)
  ) |>
  # Modify the 'license' column
  mutate(
    license = case_when(
      str_detect(license, "Hosted License") &
        str_detect(license, "Unhosted License") ~
        "with both hosted and unhosted license",
      str_detect(license, "Hosted License") ~ "with hosted license",
      str_detect(license, "Unhosted License") ~ "with unhosted license",
      str_detect(license, "HOU-0286-2022-STR-H") ~ "Registration number",
      license == "" | is.na(license) ~ "no information",
      TRUE ~ license
    )
  ) |>
  # Convert 'host_since' to Date format
  mutate(
    host_since = as.Date(host_since, format = "%Y-%m-%d")
  )

# Use KNN method to predict review_scores_rating for the rows with missing data
df_missing <- cleaned_data[is.na(cleaned_data$review_scores_rating), ]
df_no_missing <- cleaned_data[!is.na(cleaned_data$review_scores_rating), ]

knn_model_tuning <- train(
  review_scores_rating ~ price_USD + bedrooms +
    bathrooms + accommodates,
  data = df_no_missing,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(k = c(3, 5, 7, 9))
)

knn_model <- train(
  review_scores_rating ~ price_USD + bedrooms +
    bathrooms + accommodates,
  data = df_no_missing,
  method = "knn",
  tuneGrid = expand.grid(k = 9)
)

predicted_ratings <- predict(knn_model, newdata = df_missing |>
                               select(price_USD, bedrooms, bathrooms, accommodates))

cleaned_data[
  is.na(cleaned_data$review_scores_rating),
  "review_scores_rating"
] <- round(predicted_ratings, 2)

readr::write_csv(cleaned_data, file = "./data/Airbnb_DC_cleaned_June2024.csv")
