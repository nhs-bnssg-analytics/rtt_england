library(NHSRtt)
library(dplyr)
library(lubridate)
library(ggplot2)

calibration_start <- as.Date("2024-01-01")
calibration_end <- as.Date("2024-12-31")
# validation_start <- calibration_end + 1
# validation_end <- as.Date("2024-10-31")
prediction_start <- calibration_end + 1
prediction_end <- as.Date("2034-12-31")
max_months_waited <- 4 # I am only interested in waiting time bins up to 12 months

period_lkp <- dplyr::tibble(
  period = seq(
    from = calibration_start,
    to = prediction_end,
    by = "months"
  )
) |>
  mutate(
    period_id = dplyr::row_number()
  )


# get the data

# each metric must be downloaded separately
monthly_rtt <- NHSRtt::get_rtt_data(
  date_start = calibration_start,
  date_end = calibration_end,
  specialty_codes = "C_999",
  show_progress = TRUE # can change this to TRUE to see progress
)

# aggregate
monthly_rtt <- monthly_rtt |>
  mutate(
    months_waited_id = convert_months_waited_to_id(
      months_waited,
      max_months_waited
    )
  ) |>
  summarise(
    value = sum(value),
    .by = c(
      period,
      type,
      months_waited_id
    )
  ) |>
  arrange(
    type,
    months_waited_id,
    period
  ) |>
  mutate(
    period_id = dplyr::row_number(), # we need period_id for later steps
    .by = c(
      type,
      months_waited_id
    )
  )

# set up the data for modelling
referrals <- monthly_rtt |>
  filter(
    type == "Referrals"
  ) |>
  distinct(
    period_id,
    value
  ) |>
  rename(
    referrals = "value"
  )

completes <- monthly_rtt |>
  filter(
    type == "Complete"
  ) |>
  distinct(
    period_id,
    months_waited_id,
    value
  ) |>
  rename(
    treatments = "value"
  )

incompletes <- monthly_rtt |>
  filter(
    type == "Incomplete"
  ) |>
  distinct(
    period_id,
    months_waited_id,
    value
  ) |>
  rename(
    incompletes = "value"
  )


# calibrate model
params <- calibrate_capacity_renege_params(
  referrals = referrals,
  completes = completes,
  incompletes = incompletes,
  max_months_waited = max_months_waited,
  redistribute_m0_reneges = FALSE,
  full_breakdown = FALSE # this can be set to TRUE to see all the transitions for all months waited at each time step
)


# calculate referrals starting value (which will remain flat for the forecasting
# period)

ref_lm_fit <- referrals %>%
  lm(referrals ~ period_id, data = .)

p_val <- broom::tidy(ref_lm_fit) |>
  filter(term == "period_id") |>
  pull(p.value)

if (p_val > 0.05) {
  ref_start <- mean(referrals$referrals)
} else {
  ref_start <- predict(
    ref_lm_fit,
    newdata = dplyr::tibble(
      period_id = 12
    )
  )
}

ref_projections <- rep(
  ref_start,
  interval(
    prediction_start,
    prediction_end
  ) %/% months(1) + 1
)

start_capacity <- completes |>
  summarise(val = sum(treatments),
            .by = period_id) |>
  filter(period_id == max(period_id)) |>
  pull(val)

# optimise
cap_change <- optimise_capacity(
  t_1_capacity = start_capacity,
  referrals_projections = ref_projections,
  incomplete_pathways = incompletes |>
    filter(period_id == max(period_id)) |>
    select(months_waited_id, incompletes),
  renege_capacity_params = params,
  target = "8%",
  target_bin = 4,
  tolerance = 0.001,
  capacity_profile = "flat",
  max_iterations = 35
)

# projections

future_performance <- apply_params_to_projections(
  capacity_projections = rep(start_capacity * cap_change, length(ref_projections)),
  referrals_projections = ref_projections,
  incomplete_pathways = incompletes |>
    filter(period_id == max(period_id)) |>
    select(months_waited_id, incompletes),
  renege_capacity_params = params,
  max_months_waited = max_months_waited
) |>
  mutate(
    period_id = period_id + max(monthly_rtt$period_id)
  ) |>
  left_join(
    period_lkp,
    by = join_by(
      period_id
    )
  )

# charts

# wl size
wl_chart <- future_performance |>
  summarise(
    wl = sum(incompletes),
    .by = period
  ) |>
  ggplot(
    aes(
      x = period,
      y = wl
    )
  ) +
  geom_line() +
  geom_text(
    data = . %>% filter(period == max(period)),
    aes(label = scales::label_comma()(wl)),
    vjust = -1
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, NA)
  ) +
  labs(
    title = "Waiting list size based on flat referrals and flat capacity",
    subtitle = "92% performance is achieved in December 2034",
    y = "Number of people on the waiting list",
    x = ""
  )

ggsave(
  filename = "outputs/wl_chart.png",
  plot = wl_chart,
  width = 8,
  height = 4,
  units = "in",
  bg = "white"
)

performance_chart <- future_performance |>
  mutate(
    perf = case_when(
      months_waited_id < 4 ~ "Below",
      .default = "Above"
    )
  ) |>
  summarise(
    incompletes = sum(incompletes),
    .by = c(
      period, perf
    )
  ) |>
  mutate(
    prop = incompletes / sum(incompletes),
    .by = c(period)
  ) |>
  filter(
    perf == "Below"
  ) |>
  ggplot(
    aes(
      x = period,
      y = prop
    )
  ) +
  geom_line() +
  geom_text(
    data = . %>% filter(period == max(period)),
    aes(label = scales::label_percent(accuracy = 0.1)(prop)),
    vjust = -1
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    title = "4 month performance based on flat referrals and flat capacity",
    subtitle = "92% performance is achieved in December 2034",
    y = "Proportion of people on waiting list\nwhose clock stop is under 4 months",
    x = ""
  )

ggsave(
  filename = "outputs/performance_chart.png",
  plot = performance_chart,
  width = 8,
  height = 4,
  units = "in",
  bg = "white"
)


# capacity and referrals
observed <- monthly_rtt |>
  filter(type %in% c("Complete", "Referrals")) |>
  summarise(
    value = sum(value),
    .by = c(type, period)
  ) |>
  mutate(
    time = "Observed"
  )

projected <- bind_rows(
  dplyr::tibble(
    type = "Complete",
    value = rep(start_capacity * cap_change, length(ref_projections)),
    period = seq(
      from = prediction_start,
      to = prediction_end,
      by = "months"
    )
  ),
  dplyr::tibble(
    type = "Referrals",
    value = ref_projections,
    period = seq(
      from = prediction_start,
      to = prediction_end,
      by = "months"
    )
  )
) |>
  mutate(
    time = "Projected"
  )

inputs_chart <- bind_rows(
  observed,
  projected
) |>
  mutate(
    type = case_when(
      type == "Complete" ~ "Capacity (clock stops)",
      .default = type
    )
  ) |>
  ggplot(
    aes(
      x = period,
      y = value
    )
  ) +
  geom_line(
    aes(
      group = time,
      linetype = time
    )
  ) +
  theme_bw() +
  scale_linetype_manual(
    name = "",
    values = c(
      Observed = "solid",
      Projected = "dashed"
    )
  ) +
  facet_wrap(
    facets = vars(type),
    ncol = 2
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Past and future capacity and referrals to achieve a stable 92% performance by Dec 2034",
    y = "Monthly count of clock stops/referrals",
    x = ""
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = "outputs/inputs_chart.png",
  plot = inputs_chart,
  width = 10,
  height = 5,
  units = "in",
  bg = "white"
)

