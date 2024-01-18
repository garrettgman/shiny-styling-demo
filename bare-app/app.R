library(shiny)
library(tidyverse)

source("setup.R")
source("helpers.R")

# Build the app UI
ui <- fluidPage(
  titlePanel("Effectiveness of DemoCo App Free Trial by Customer Segment"),
  sidebarLayout(
    sidebarPanel(
      title = "Select a segment of data to view",
      sidebar_content
    ),
    mainPanel(
      fluidRow(
        column(width = 8,
               wellPanel("Conversions over time",
                         plotOutput("line"))),
        column(width = 4,
               wellPanel("Conversion rates",
                         plotOutput("bar")))
      ),
      fluidRow(
        column(width = 4,
               wellPanel("Recommended Trial",
                         textOutput("recommended_eval"))),
        column(width = 4,
               wellPanel("Customers",
                         textOutput("number_of_customers"))),
        column(width = 4,
               wellPanel("Avg Spend",
                         textOutput("average_spend")))
      ),
      wellPanel("Conversion rates by subgroup",
                tableOutput("table"))
    )
  )
)


# Define the Shiny server function
server <- function(input, output) {

  # Filter data according to inputs
  selected_industries <-
    reactive(if (is.null(input$industries)) industries else input$industries)

  selected_propensities <-
    reactive(if (is.null(input$propensities)) propensities else input$propensities)

  selected_contracts <-
    reactive(if (is.null(input$contracts)) contracts else input$contracts)

  selected_data <-
    reactive({
      filter_customers(selected_industries(),
                       selected_propensities(),
                       selected_contracts())
    })

  selected_data_by_group <-
    reactive({
      filter_customers_by_group(selected_industries(),
                                selected_propensities(),
                                selected_contracts())
    })


  # Make plots
  output$line <-
    renderPlot({
      plot_conversions_over_time(selected_data())
    })

  output$bar <-
    renderPlot({
      plot_conversions_by_group(selected_data_by_group())
    })


  # Compute values for value boxes
  output$recommended_eval <-
    renderText({
      choose_recommendation(selected_data())
    })

  output$number_of_customers <-
    renderText({
      count_customers(selected_data())
    })

  output$average_spend <-
    renderText({
      compute_average_spend(selected_data())
    })


  # Render table
  output$table <-
    renderTable(digits = 0, {
      make_table(selected_data_by_group())
    })
}


# Create the Shiny app
shinyApp(ui = ui, server = server)
