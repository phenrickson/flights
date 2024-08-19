# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "tidymodels",
    "glmnet",
    "bonsai",
    "lightgbm"
  ),
  # default format for storing targets
  format = "qs",
  # set memory to transient
  memory = "transient",
  repository = "local"
)

# Run the R scripts in the R/ folder with your custom functions:
suppressWarnings({tar_source("src")})
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # data
  tar_target(
    name = flights,
    command = nycflights13::flights
  ),
  tar_target(
    name = weather,
    command = nycflights13::weather
  ),
  tar_target(
    name = flights_prepared,
    command = flights |> prepare_flights()
  ),
  tar_target(
    name = weather_prepared,
    command = weather |> prepare_weather()
  ),
  tar_target(
    full_data,
    flights_prepared |>
    inner_join(weather_prepared)
  ),
  # split
  tar_target(
    split,
    full_data |>
    initial_validation_split(strata = arr_delay)
  ),
  # extract training set
  tar_target(
    train_data,
    split |>
    training()
  ),
  # test set; do not touch until the end
  tar_target(
    test_data,
    split |>
    testing()
  ),
  # plot split
  tar_target(
    plot_split,
    bind_rows(
      split |>
      training() |>
      mutate(type = "training"),
      split |>
      validation() |>
      mutate(type = "validation"),
      split |>
      testing() |>
      mutate(type = "testing"),
    ) |>
    mutate(type = factor(type, levels = c("testing", "validation", "training"))) |>
    group_by(date = floor_date(date, "month"), type) |>
    count() |>
    ggplot(aes(x = date, y = n, fill = type)) +
    geom_col() +
    scale_fill_viridis_d()
  ),
  # examine missigness
  tar_target(
    plot_missing,
    train_data |>
    naniar::vis_miss(warn_large_data = F)
  ),
  # add settings for model tuning/eval
  tar_target(
    my_ctrl,
    control_resamples(
      event_level = "second",
      verbose = T,
      save_workflow = T
    ),
  ),
  tar_target(
    my_metrics,
    metric_set(
      yardstick::roc_auc,
      yardstick::pr_auc,
      yardstick::mn_log_loss
    )
  ),
  # add simple model as a baseline
  tar_target(
    baseline_wflow,
    workflow() |>
    add_model(
      logistic_reg()
    ) |>
    add_recipe(
      recipe(
        arr_delay ~ air_time + distance + dep_time,
        data = train_data
      ) |>
      step_impute_median(
        all_numeric_predictors()
      )
    )
  ),
  # evaluate model on validation set
  tar_target(
    baseline_tuned,
    baseline_wflow |>
    tune_grid(
      resamples = split |>
      validation_set(),
      control = my_ctrl,
      metrics = my_metrics
    )
  ),
  # extract results
  tar_target(
    name = baseline_results,
    command =
    baseline_tuned |>
    collect_metrics() |>
    mutate(wflow_id = 'baseline_glm')
  ),
  # bind together model metrics
  tar_target(
    name = valid_metrics,
    command =
    bind_rows(
      baseline_results
    ) |>
    select(wflow_id, everything()) |>
    pivot_metrics()
  ),
  # write metrics to repository
  tar_target(
    write_metrics,
    valid_metrics |>
    write_csv("targets-runs/valid_metrics.csv")
  )
)
