# Build the app UI
ui <- fluidPage(
  titlePanel("Effectiveness of DemoCo App Free Trial by Customer Profile"),
  sidebarLayout(
    sidebarPanel(
      sidebar_content
    ),
    mainPanel(
      fluidRow(
        column(
          width = 8,
          wellPanel(
            "Conversions over time",
            plotOutput("line")
          )
        ),
        column(
          width = 4,
          wellPanel(
            "Conversion rates",
            plotOutput("bar")
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            "Recommended Trial",
            textOutput("recommended_eval")
          )
        ),
        column(
          width = 4,
          wellPanel(
            "Users",
            textOutput("number_of_users")
          )
        ),
        column(
          width = 4,
          wellPanel(
            "Avg Spend",
            textOutput("average_spend")
          )
        )
      ),
      wellPanel(
        "Conversion rates by subgroup",
        tableOutput("table")
      )
    )
  )
)