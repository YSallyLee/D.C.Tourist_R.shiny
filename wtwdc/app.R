library(shiny)
library(bslib)
library(DT)
library(leaflet)
library(sf)
library(tidyverse)

source("./intro_html.R")

## Read in Data ------------------

cleaned_data <- read.csv("../data/Airbnb_DC_cleaned_June2024.csv")
landmarks <- read.csv("../data/landmarks.csv")

## Function create

create_prediction_model <- function(data) {
  # Create a linear model for price prediction
  model <- lm(
    price_USD ~ property_group + accommodates + bathrooms + bedrooms
      + review_scores_rating,
    data = data
  )
  return(model)
}

predict_price_range <- function(model, new_data) {
  # Make prediction
  predicted <- predict(model, newdata = new_data, interval = "prediction")

  # Return the lower and upper bounds of the prediction interval
  return(list(
    lower = max(0, predicted[, "lwr"]),
    fit = predicted[, "fit"],
    upper = predicted[, "upr"]
  ))
}

###
### Enter Business Logic before this line
###

###
## Begin User Interface Section ----------------
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "sketchy"),
  titlePanel("Welcome to Washington, D.C."),
  tabsetPanel(

    # Tab 1: Introduction
    tabPanel("Introduction", fluidRow(column(8, intro_content))),

    # Tab 2: Find Your Ideal DC Neighborhood
    tabPanel(
      "Find Your Ideal DC Neighborhood",
      sidebarLayout(
        sidebarPanel(
          h3("Select Area"),
          selectInput("neighbourhood", "Choose a neighborhood",
            choices = c(
              "Select Neighborhood" = "",
              sort(unique(as.character(
                cleaned_data$neighbourhood_cleansed
              )))
            ),
            multiple = FALSE
          ),
          hr(),
          # Additional controls can be added here
          helpText("Click on an attraction to see nearby neighborhoods")
        ),
        mainPanel(
          # Map Panel
          wellPanel(
            h4("Attraction & Neighborhood Map"),
            leafletOutput("map", height = "400px")
          ),
          # Data Overview Panel
          wellPanel(
            h4("Airbnb Data Overview"),
            # Price Distribution
            wellPanel(
              h4("Price Distribution"),
              plotOutput("priceBoxPlot")
            ),
            fluidRow(
              # Property Types
              column(
                4,
                wellPanel(
                  h4("Property Types"),
                  plotOutput("propertyTypePie")
                )
              ),
              # Bathrooms
              column(
                4,
                wellPanel(
                  h4("Bathrooms"),
                  plotOutput("bathroomsPie")
                )
              ),
              # Bedrooms
              column(
                4,
                wellPanel(
                  h4("Bedrooms"),
                  plotOutput("bedroomsPie")
                )
              )
            )
          )
        )
      )
    ),

    # Tab 3: Reasonable Price Prediction
    tabPanel(
      "Price Prediction",
      sidebarLayout(
        sidebarPanel(
          h4("Accommodation Preferences"),
          # Update choices based on unique values in your data
          selectInput("propertyGroup", "Property Type",
            choices = unique(cleaned_data$property_group)
          ),
          numericInput("guests", "Number of Guests",
            value = 2, min = 1, max = max(cleaned_data$accommodates)
          ),
          sliderInput("bathrooms", "Number of Bathrooms",
            min = min(cleaned_data$bathrooms, na.rm = TRUE),
            max = max(cleaned_data$bathrooms, na.rm = TRUE),
            value = 1, step = 0.5
          ),
          sliderInput("bedrooms", "Number of Bedrooms",
            min = min(cleaned_data$bedrooms, na.rm = TRUE),
            max = max(cleaned_data$bedrooms, na.rm = TRUE),
            value = 1, step = 1
          ),
          sliderInput("rating", "Minimum Review Score",
            min = min(cleaned_data$review_scores_rating, na.rm = TRUE),
            max = max(cleaned_data$review_scores_rating, na.rm = TRUE),
            value = 4, step = 0.1
          )
        ),
        mainPanel(
          h3("Estimated Price Prediction"),
          textOutput("predicted_price"),
          br(),
          h4("Price Prediction Model Overview"),
          p("This model predicts the price of a property based on various features, including:"),
          tags$ul(
            tags$li("Property Group: The type of property (e.g., private room, entire properties)."),
            tags$li("Accommodates: The number of people the property can accommodate."),
            tags$li("Bathrooms: The number of bathrooms in the property."),
            tags$li("Bedrooms: The number of bedrooms in the property."),
            tags$li("Review Score Rating: The average rating given by past guests.")
          ),
          p("The model uses a linear regression approach, which means it finds the relationship between these features and the price of the property to make predictions."),
          br(),
          h4("Price Prediction Range"),
          p("The price prediction comes with a range to reflect the uncertainty in the estimation. The range consists of:"),
          tags$ul(
            tags$li("Predicted Price: This is the model's best estimate of the property's price based on the input features."),
            tags$li("Lower Bound: This is the lower end of the predicted price range, indicating the lowest price the model expects based on the data."),
            tags$li("Upper Bound: This is the upper end of the predicted price range, showing the highest price the model expects.")
          ),
          p("By providing this range, we account for potential variability and give a more realistic sense of the price you might expect.")
        )
      )
    ),


    # Tab 4: Conclusion
    tabPanel(
      "Conclusion",
      fluidRow(
        column(12, wellPanel(
          h3("Interactive Map"),
          p(HTML("We have marked the Airbnb options that meet your accommodation criteria and are within a reasonable price range with pink markers on the map below. The light blue markers indicate the locations of attractions.
            <br><br>Click on the pink markers to view detailed information about the Airbnb listings, and click on the blue markers to see the name and photos of the attractions.")),
          leafletOutput("recommended_map", height = "600px")
        )),
        column(12, wellPanel(
          h3("Recommended Listings"),
          p(HTML("Below is a detailed list of the recommended Airbnb options from the map above:
            <br><br>Click on the column headers to sort the list based on that field.")),
          dataTableOutput("recommended_list")
        ))
      ) # end of fluidRow
    ) # end of tabPanel
  ) # end of tabsetPanel
) # end of ui

server <- function(input, output, session) {
  # Attraction Map
  filtered_data <- reactive({
    req(input$neighbourhood)
    cleaned_data |>
      filter(neighbourhood_cleansed == input$neighbourhood)
  })

  # Dynamically update map with filtered Airbnb data
  observe({
    data <- filtered_data()

    # Update only Airbnb markers without affecting landmarks
    leafletProxy("map", data = data) |>
      clearGroup("airbnb") |> # Clear only the "airbnb" group
      addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ paste0(
          "<strong>", name, "</strong><br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude
        ),
        group = "airbnb" # Add markers to the "airbnb" group
      )
  })

  # Create Map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(
        lng = mean(cleaned_data$longitude),
        lat = mean(cleaned_data$latitude),
        zoom = 12
      ) |>
      # Add landmarks during initial map creation
      addAwesomeMarkers(
        data = landmarks,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = "star",
          markerColor = "orange",
          iconColor = "white"
        ),
        popup = ~ paste0(
          "<strong>", Landmark, "</strong><br>",
          "<img src='", url, "' width='120' height='100'>"
        )
      )
  })

  # Dynamically update map with filtered Airbnb data
  observe({
    data <- filtered_data()

    # Update only Airbnb markers without affecting landmarks
    leafletProxy("map", data = data) |>
      clearGroup("airbnb") |> # Clear only the "airbnb" group
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ paste0(
          "<strong>", name, "</strong><br>",
          "Latitude: ", latitude, "<br>",
          "Longitude: ", longitude
        ),
        group = "airbnb" # Add markers to the "airbnb" group
      )
  })

  # Price Distribution Box Plot
  output$priceBoxPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = price_USD)) +
      geom_boxplot() +
      theme_minimal()
  })

  # Property Types Pie Chart
  output$propertyTypePie <- renderPlot({
    data <- filtered_data()
    data |>
      count(property_group) |>
      ggplot(aes(x = "", y = n, fill = property_group)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = paste0(
          property_group, "\n",
          scales::percent(n / sum(n))
        )),
        position = position_stack(vjust = 0.5)
      ) +
      theme_minimal() +
      labs(fill = "Type") +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  # Bathrooms Distribution Pie Chart
  output$bathroomsPie <- renderPlot({
    data <- filtered_data()
    data |>
      count(bathrooms) |>
      ggplot(aes(x = "", y = n, fill = factor(bathrooms))) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = scales::percent(n / sum(n))),
        position = position_stack(vjust = 0.5),
        size = 3
      ) +
      theme_minimal() +
      labs(fill = "Number of Bathrooms") +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  # Bedrooms Distribution Pie Chart
  output$bedroomsPie <- renderPlot({
    data <- filtered_data()
    data |>
      count(bedrooms) |>
      ggplot(aes(x = "", y = n, fill = factor(bedrooms))) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = scales::percent(n / sum(n))),
        position = position_stack(vjust = 0.5),
        size = 3
      ) +
      theme_minimal() +
      labs(fill = "Number of Bedrooms") +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  # Create the prediction model when the app starts
  price_model <- reactive({
    create_prediction_model(cleaned_data)
  })

  # Create reactive prediction
  price_prediction <- reactive({
    req(
      input$propertyGroup, input$guests, input$bathrooms,
      input$bedrooms, input$rating
    )

    # Create a new data frame with the input values
    new_data <- data.frame(
      property_group = input$propertyGroup, # No need for [1] since it's now a single selection
      accommodates = input$guests,
      bathrooms = input$bathrooms,
      bedrooms = input$bedrooms,
      review_scores_rating = input$rating
    )

    # Get prediction
    predict_price_range(price_model(), new_data)
  })

  # Display the prediction
  output$predicted_price <- renderText({
    pred <- price_prediction()

    sprintf(
      "Based on your selections, we estimate the price range to be:
      $%.2f - $%.2f per night
      (Average: $%.2f)",
      pred$lower,
      pred$upper,
      pred$fit
    )
  })

  # Recommended Data

  recommended_data <- reactive({
    pred <- price_prediction()

    cleaned_data |>
      filter(neighbourhood_cleansed == input$neighbourhood) |>
      filter(property_group %in% input$propertyGroup) |>
      filter(accommodates == input$guests) |>
      filter(bathrooms == input$bathrooms) |>
      filter(bedrooms == input$bedrooms) |>
      filter(review_scores_rating >= input$rating) |>
      filter(price_USD >= pred$lower & price_USD <= pred$upper)
  })

  # Interactive Map- Recommended
  output$recommended_map <- renderLeaflet({
    # Ensure there is data
    if (nrow(recommended_data()) == 0) {
      return(leaflet() |>
        addTiles() |>
        addPopups(
          lng = 0, lat = 0, popup = "No data available.<br><br>Please choose a different neighborhood or modify your ideal accommodation criteria."
        ))
    }

    # Create the map
    leaflet(data = recommended_data()) |>
      addTiles() |>
      addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = awesomeIcons(
          markerColor = "pink",
          iconColor = "white"
        ),
        popup = ~ paste0(
          "<b>Name:</b> ", name, "<br>",
          "<b>Price (USD):</b> $", price_USD, "<br>",
          "<b>Review Score:</b> ", review_scores_rating, "<br>",
          "<b>URL:</b> <a href='", listing_url,
          "' target='_blank'>", listing_url, "</a>"
        )
      ) |>
      addAwesomeMarkers(
        data = landmarks,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = awesomeIcons(
          icon = "star",
          markerColor = "blue",
          iconColor = "white"
        ),
        popup = ~ paste0(
          "<strong>", Landmark, "</strong><br>",
          "<img src='", url, "' width='120' height='100'>"
        )
      )
  })

  # Recommended Listings
  output$recommended_list <- renderDT({
    datatable(
      recommended_data(),
      options = list(
        scrollX = TRUE
      )
    )
  })
}
# end of server

shinyApp(ui, server)
