library(tidyverse)
library(tidymodels)
library(bonsai)
library(lightgbm)

#renv::install('nycflights13')

flights = nycflights13::flights
weather = nycflights13::weather


add_delay = function(data) {

    data |>
        mutate(arr_delay = case_when(arr_delay >=30 ~ 'late',
                                     TRUE ~ 'on_time'),
               arr_delay = factor(arr_delay, levels = c("on_time", "late"))
        )
}

add_date = function(data) {

    data |>
        mutate(date = lubridate::as_date(time_hour))
}

full_data =
    flights |>
    add_delay() |>
    add_date() |>
    inner_join(
        weather |>
            select(origin, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib, time_hour)
    ) |>
    select(
        date,
        year,
        month,
        day,
        hour,
        minute,
        arr_delay,
        sched_dep_time,
        sched_arr_time,
        dep_time,
        arr_time,
        carrier,
        flight,
        tailnum,
        origin,
        dest,
        air_time,
        distance,
        hour,
        minute,
        temp,
        dewp,
        humid,
        wind_dir,
        wind_speed,
        wind_gust,
        precip,
        pressure,
        visib
    )

set.seed(1)
split =
    full_data |>
    initial_validation_split(strata = arr_delay)

split |>
    training() |>
    select(ends_with("_time"), arr_delay) |>
    mutate(arr_time - dep_time)

split |>
    training() |>
    naniar::vis_miss(warn_large_data = F)

base_recipe =
    split |>
    training() |>
    recipe(
        arr_delay ~ .,
        data = _
    ) |>
    # set everything to ID by default
    update_role(everything(),
                -all_outcomes(),
                new_role = "id") |>
    # set other predictors
    add_role(
        date,
        dep_time,
        carrier,
        origin,
        dest,
        air_time,
        distance,
        new_role = "predictor"
    ) |>
    # craeate features for dates
    step_date(date, features = c("dow", "month")) |>
    step_holiday(date,
                 holidays = timeDate::listHolidays("US"),
                 keep_original_cols = FALSE
    ) |>
    # impute numeric
    step_impute_median(
        all_numeric_predictors()
    ) |>
    # nominal variables
    # novel
    step_novel(
        all_nominal_predictors()
    ) |>
    # unknown
    step_unknown(
        all_nominal_predictors()
    ) |>
    # dummy
    step_dummy(
        all_nominal_predictors()
    ) |>
    # remove zero variance
    step_zv(all_predictors())

glm_mod = logistic_reg()
glmnet_mod = logistic_reg(penalty = tune::tune(),
                          mixture = tune::tune()) |>
    set_engine(engine = "glmnet")

glmnet_grid = expand_grid(penalty = 10^seq(-3, -0.75, length = 10),
                          mixture = c(0, 0.5, 1))

cart_mod = decision_tree()

lightgbm_mod =
    parsnip::boost_tree(
        mode = "classification",
        trees = trees,
        min_n = tune(),
        tree_depth = tune()
    ) |>
    set_engine("lightgbm",
               objective = "binary")

lightgbm_grid =
    grid_max_entropy(
        x = dials::parameters(
            min_n(), # 2nd important
            tree_depth() # 3rd most important
        ),
        size = 10
    )

base_wflow =
    workflow() |>
    add_model(glm_mod) |>
    add_recipe(
        recipe(
            arr_delay ~ air_time + distance,
            data = split |> training()
        ) |>
            step_impute_median(
                all_numeric_predictors()
            )
    )

glmnet_wflow =
    workflow() |>
    add_model(glmnet_mod) |>
    add_recipe(
        base_recipe |>
            step_normalize(all_numeric_predictors())
    )

lightgbm_wflow =
    workflow() |>
    add_model(lightgbm_mod) |>
    add_recipe(
        base_recipe
    )

my_ctrl =
    control_resamples(event_level = 'second',
                      verbose = T,
                      save_workflow = T)

my_metrics =
    metric_set(
        yardstick::roc_auc,
        yardstick::pr_auc,
        yardstick::mn_log_loss
    )

base_fit =
    base_wflow |>
    tune_grid(
        resamples = split |>
            validation_set(),
        control = my_ctrl,
        metrics = my_metrics
    )

glmnet_tuned =
    glmnet_wflow |>
    tune_grid(
        grid = glmnet_grid,
        resamples = split |>
            validation_set(),
        control = my_ctrl,
        metrics = my_metrics
    )

lightgbm_tuned =
    lightgbm_wflow |>
    tune_grid(
        grid = 10,
        resamples = split |>
            validation_set(),
        control = my_ctrl,
        metrics = my_metrics
    )

wflow_set =
    as_workflow_set("glm_airtime+distance" = base_fit,
                    "glmnet_flight"= glmnet_tuned,
                    "lightgbm_flight" = lightgbm_tuned)

wflow_set |>
    autoplot(type = 'wflow_id',
             metric = "mn_log_loss")

best_mod =
    wflow_set |>
    fit_best(metric = 'mn_log_loss')

best_mod |>
    tidy() |>
    filter(term != "(Intercept)") |>
    filter(estimate != 0) |>
    slice_max(abs(estimate), n =50) |>
    ggplot(aes(x=estimate,
               y=reorder(term, estimate)))+
    geom_point()

test_preds =
    best_mod |>
    augment(
        split |>
            testing()
    )

test_preds |>
    select(.pred_class, .pred_late, arr_delay) |>
    ggplot(aes(x=.pred_late))+
    geom_histogram(bins = 50)+
    facet_wrap(arr_delay ~.,
               ncol = 1)

test_preds |>
    roc_curve(
        truth = arr_delay,
        .pred_late,
        event_level = 'second'
    ) |>
    autoplot()

glmnet_fit =
    glmnet_tuned |>
    fit_best(metrioc )

autoplot(glmnet_tuned)

lightgbm_tuned =


    base_fit |>
    collect_metrics()

glmnet_fit |>
    collect_metrics()

split |>
    training() |>
    group_by(minute, arr_delay) |>
    count() |>
    group_by(minute) |>
    mutate(prop = n / sum(n)) |>
    ggplot(aes(x=minute, y=prop, fill = arr_delay))+
    geom_col()

flights_recipe <- recipe(
    arr_delay ~ .,
    data = train_data
) %>%
    update_role(flight, time_hour, new_role = "ID")
summary(flights_recipe)


set.seed(1234)
flight_data <- flights %>%
    mutate(
        # Convert the arrival delay to a factor
        arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
        arr_delay = factor(arr_delay),

        # We will use the date (not date-time) in the recipe below
        date = lubridate::as_date(time_hour)
    ) %>%
    # Include the weather data
    inner_join(weather, by = c("origin", "time_hour")) %>%

    # Only retain the specific columns we will use
    select(
        dep_time, flight, origin, dest, air_time, distance, carrier, date,
        arr_delay, time_hour
    ) %>%
    na.omit() %>% # Exclude missing data
    mutate_if(is.character, as.factor)
