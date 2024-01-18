library(shiny)
library(tidyverse)
library(bslib)

source("setup.R")
source("helpers.R")

# Build the app UI
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "darkly",
                   bg = "#222222",
                   fg = "#86C7ED"),
  title = "Effectiveness of DemoCo App Free Trial by Customer Segment",
  sidebar = sidebar(
    title = "Select a segment of data to view",
    class = "fg-primary",
    sidebar_content,
    tags$img(src = "logo.png", width = "100%", height = "auto")
  ),
  layout_columns(
    card(card_header("Conversions over time"),
              plotOutput("line")),
    card(card_header("Conversion rates"),
              plotOutput("bar")),
    value_box("Recommended Trial",
              textOutput("recommended_eval"),
              theme_color = "secondary"),
    value_box("Customers",
              textOutput("number_of_customers"),
              theme_color = "secondary"),
    value_box("Avg Spend",
              textOutput("average_spend"),
              theme_color = "secondary"),
    card(card_header("Conversion rates by subgroup"),
              tableOutput("table")),
    col_widths = c(8,4,4,4,4,12),
    row_heights = c(4, 1.5, 3)
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
