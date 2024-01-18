# helpers.R

choose_recommendation <- function(data) {
  recommendation <-
    data |>
    group_by(evaluation) |>
    summarise(rate = mean(outcome == "Won"), .groups = "drop") |>
    filter(rate == max(rate)) |>
    pull(evaluation)

  as.character(recommendation[1])
}

compute_average_spend <- function(data){
  x <-
    data |>
    filter(outcome == "Won") |>
    summarise(spend = round(mean(amount))) |>
    pull(spend)

  str_glue("${x}")
}

count_users <- function(data) {
  sum(data$outcome == "Won") |>
    format(big.mark = ",")
}

filter_users <- function(industries, propensities, contracts) {

  users |>
    filter(industry %in% industries,
           propensity %in% propensities,
           contract %in% contracts)
}

filter_users_by_group <- function(industries, propensities, contracts) {
  users_by_group |>
    filter(industry %in% industries,
           propensity %in% propensities,
           contract %in% contracts)
}

make_table <- function(data_by_group) {
  data_by_group |>
    select(industry, propensity, contract, evaluation, success_rate) |>
    pivot_wider(names_from = evaluation, values_from = success_rate)
}

plot_conversions_by_group <- function(data_by_group){
  data_by_group |>
    group_by(evaluation) |>
    summarise(rate = round(sum(n * success_rate) / sum(n), 2)) |>
    ggplot(aes(x = evaluation, y = rate, fill = evaluation)) +
    geom_col() +
    guides(fill = "none") +
    theme(axis.title = element_blank()) +
    scale_y_continuous(limits = c(0, 100))
}

plot_conversions_over_time <- function(data){
  data |>
    mutate(date = floor_date(date, unit = "month")) |>
    group_by(date, evaluation) |>
    summarize(n = sum(outcome == "Won"), .groups = "drop") |>
    ggplot(aes(x = date, y = n, color = evaluation)) +
      geom_line() +
      theme(axis.title = element_blank()) +
      labs(color = "Trial Type")
}