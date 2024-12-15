intro_content <- tags$div(
  
  # Main content section
  tags$div(
    class = "container",
    
    # Add an image as the header
    tags$img(
      src = "Header.jpg", 
      class = "img-fluid mb-4", 
      alt = "Header Image"
    ),

    # Purpose section
    tags$div(
      class = "mb-4",
      tags$h4("Purpose", class = "text-secondary"),
      tags$p(
        "This application helps first-time visitors to Washington D.C. find the perfect Airbnb accommodation by:",
        class = "lead"
      ),
      tags$ul(
        tags$li(
          tags$i(class = "fas fa-map-marker-alt me-2"),
          "Exploring neighborhoods based on proximity to major attractions"
        ),
        tags$li(
          tags$i(class = "fas fa-chart-bar me-2"),
          "Providing insights about Airbnb options in different areas"
        ),
        tags$li(
          tags$i(class = "fas fa-dollar-sign me-2"),
          "Estimating reasonable price ranges based on your preferences"
        ),
        tags$li(
          tags$i(class = "fas fa-home me-2"),
          "Recommending specific accommodations that match your criteria"
        )
      )
    ),

    # How to Use section
    tags$div(
      class = "mb-4",
      tags$h4("How to Use This App", class = "text-secondary"),
      tags$ol(
        class = "list-group list-group-numbered mb-3",
        tags$li(
          class = "list-group-item",
          "Start by exploring DC neighborhoods in the 'Find Your Ideal DC Neighborhood' tab",
          tags$small(
            class = "d-block text-muted",
            "Browse attractions and see which neighborhoods are most convenient for your visit"
          )
        ),
        tags$li(
          class = "list-group-item",
          "Use the 'Reasonable Price Range' tab to get price estimates",
          tags$small(
            class = "d-block text-muted",
            "Input your preferences for property type, size, and amenities"
          )
        ),
        tags$li(
          class = "list-group-item",
          "View final recommendations in the 'Conclusion' tab",
          tags$small(
            class = "d-block text-muted",
            "See a curated list of Airbnb properties that match your criteria"
          )
        )
      )
    ),

    # Data Sources section
    tags$div(
      class = "mb-4",
      tags$h4("About the Data", class = "text-secondary"),
      tags$p("This application uses two main data sources:"),
      tags$div(
        class = "card-group",
        tags$div(
          class = "card m-2",
          tags$div(
            class = "card-body",
            tags$h5(
              class = "card-title",
              tags$i(class = "fas fa-landmark me-2"),
              "Tourist Attractions"
            ),
            tags$p(
              class = "card-text",
              "Data from US News '31 Best Things To Do in Washington, D.C.'",
              tags$br(),
              tags$small("Last updated: September 2024"),
              tags$br(),
              tags$a(
                href = "https://travel.usnews.com/Washington_DC/Things_To_Do/",
                target = "_blank",
                "View Data Source",
                class = "btn btn-sm btn-outline-primary mt-2"
              )
            )
          )
        ),
        tags$div(
          class = "card m-2",
          tags$div(
            class = "card-body",
            tags$h5(
              class = "card-title",
              tags$i(class = "fas fa-bed me-2"),
              "Airbnb Listings"
            ),
            tags$p(
              class = "card-text",
              "Washington, D.C. Airbnb listing data",
              tags$br(),
              tags$small("Last updated: June 21, 2024"),
              tags$br(),
              tags$a(
                href = "https://insideairbnb.com/get-the-data/",
                target = "_blank",
                "View Data Source",
                class = "btn btn-sm btn-outline-primary mt-2"
              )
            )
          )
        )
      )
    ),

    # Additional Information
    tags$div(
      class = "alert alert-info",
      tags$h5(
        tags$i(class = "fas fa-info-circle me-2"),
        "Note"
      ),
      tags$p(
        "This tool is designed to help you make informed decisions about your stay in Washington D.C. ",
        "All recommendations are based on historical data and general trends. ",
        "Actual prices and availability may vary."
      )
    )
  )
)
