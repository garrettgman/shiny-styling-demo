# Load packages
library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(gitlink)

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

# Load data, objects, and helper functions
source("setup.R")
source("helpers.R")

# Build the app UI
ui <- page_sidebar(

  # Add github link
  ribbon_css("https://github.com/garrettgman/shiny-styling-demo"),

  # Set the CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   "table-color" = "#86C7ED",
                   success = "#86C7ED"),

  # Add title
  title = "Effectiveness of DemoCo App Free Trial by Customer Profile",

  # Add sidebar elements
  sidebar = sidebar(
    class = "bg-secondary",
    sidebar_content,
    tags$img(src = "logo.png", width = "100%", height = "auto")
  ),

  # Layout non-sidebar elements
  layout_columns(
    card(card_header("Conversions over time", class = "text-success"),
         plotOutput("line")),
    card(card_header("Conversion rates", class = "text-success"),
         plotOutput("bar")),
    value_box("Recommended Trial",
              textOutput("recommended_eval"),
              theme = "secondary"),
    value_box("Customers",
              textOutput("number_of_customers"),
              theme = "secondary"),
    value_box("Avg Spend",
              textOutput("average_spend"),
              theme = "secondary"),
    card(card_header("Conversion rates by subgroup", class = "text-success"),
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
