library(shiny)
library(tidyverse)

source("setup.R")
source("helpers.R")

ui <- fluidPage(
  titlePanel("Effectiveness of DemoCo App Free Trial by Customer Profile"),
  sidebarLayout(
    sidebarPanel(
      sidebar_content
    ),
    mainPanel(
      "Conversions over time",
      plotOutput("line"),

      "Conversion rates",
      plotOutput("bar"),

      "Recommended Trial",
      textOutput("recommended_eval"),

      "Users",
      textOutput("number_of_users"),

      "Avg Spend",
      textOutput("average_spend"),

      "Conversion rates by subgroup",
      tableOutput("table")
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
      filter_users(selected_industries(),
                       selected_propensities(),
                       selected_contracts())
    })

  selected_data_by_group <-
    reactive({
      filter_users_by_group(selected_industries(),
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

  output$number_of_users <-
    renderText({
      count_users(selected_data())
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
