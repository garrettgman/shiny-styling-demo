# setup.R

# Read and prepare data
customers <-
  read_csv("data/customers.csv") |>
  mutate(evaluation = factor(evaluation, levels = c("None", "A", "B")),
         propensity = factor(propensity, levels = c("Good", "Average", "Poor")))

customers_by_group <-
  customers |>
  group_by(industry, propensity, contract, evaluation) |>
  summarize(success_rate = round(mean(outcome == "Won")* 100),
            avg_amount = round(mean(amount)),
            avg_days = round(mean(days)),
            n = n(),
            .groups = "drop")


# Make layout elements
industries = c("Academia",
               "Energy",
               "Finance",
               "Government",
               "Healthcare",
               "Insurance",
               "Manufacturing",
               "Non-Profit",
               "Pharmaceuticals",
               "Technology")
propensities = c("Good", "Average", "Poor")
contracts = c("Monthly", "Annual")

sidebar_content <-
  list(
    selectInput("industries",
                "Select industries",
                choices = industries,
                selected = "",
                multiple  = TRUE),
    selectInput("propensities",
                "Select propensities to buy",
                choices = propensities,
                selected = "",
                multiple  = TRUE),
    selectInput("contracts",
                "Select contract types",
                choices = contracts,
                selected = "",
                multiple  = TRUE),
  "This app compares the effectiveness of two types of free trials, A (30-days) and B (100-days), at converting users into customers."
)