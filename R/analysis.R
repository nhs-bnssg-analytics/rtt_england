library(NHSRtt)
library(dplyr)
library(lubridate)
library(ggplot2)

calibration_start <- as.Date("2024-01-01")
calibration_end <- as.Date("2024-12-31")
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
    time = "Observed",
    adjustment = "Observed"
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
    time = "Projected",
    adjustment = case_when(
      type == "Referrals" ~ "Unadjusted",
      .default = "Observed"
    )
  )

projected_adjusted <- projected |>
  filter(
    type == "Referrals"
  ) |>
  mutate(
    value = value + abs(value * params$renege_param[[1]]),
    adjustment = "Adjusted"
  )

inputs_chart <- bind_rows(
  observed,
  projected,
  projected_adjusted
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
      group = interaction(adjustment, time),
      linetype = time,
      colour = adjustment
    )
  ) +
  geom_text(
    data = . %>% filter(period == max(period), type == "Referrals"),
    aes(label = adjustment,
        colour = adjustment),
    hjust = 1,
    vjust = 1.5
  ) +
  theme_bw() +
  scale_linetype_manual(
    name = "",
    values = c(
      Observed = "solid",
      Projected = "dashed"
    )
  ) +
  scale_colour_manual(
    name = "",
    values = c(
      Observed = "black",
      Unadjusted = "gray70",
      Adjusted = "black"
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
  theme(legend.position = "bottom") +
  guides(colour = "none")

ggsave(
  filename = "outputs/inputs_chart.png",
  plot = inputs_chart,
  width = 10,
  height = 5,
  units = "in",
  bg = "white"
)


# same analysis by specialty

# get the data

# each metric must be downloaded separately
monthly_rtt <- NHSRtt::get_rtt_data(
  date_start = calibration_start,
  date_end = calibration_end,
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
      specialty,
      period,
      type,
      months_waited_id
    )
  ) |>
  arrange(
    specialty,
    type,
    months_waited_id,
    period
  ) |>
  mutate(
    period_id = dplyr::row_number(), # we need period_id for later steps
    .by = c(
      specialty,
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
    specialty,
    period_id,
    value
  ) |>
  rename(
    referrals = "value"
  ) |>
  tidyr::nest(
    referrals_data = c(period_id, referrals)
  )

completes <- monthly_rtt |>
  filter(
    type == "Complete"
  ) |>
  distinct(
    specialty,
    period_id,
    months_waited_id,
    value
  ) |>
  rename(
    treatments = "value"
  ) |>
  tidyr::nest(
    completes_data = c(period_id, months_waited_id, treatments)
  )

incompletes <- monthly_rtt |>
  filter(
    type == "Incomplete"
  ) |>
  distinct(
    specialty,
    period_id,
    months_waited_id,
    value
  ) |>
  rename(
    incompletes = "value"
  ) |>
  tidyr::nest(
    incompletes_data = c(period_id, months_waited_id, incompletes)
  )

# put data together and calculate parameters
all_specialty_calibration_data <- referrals |>
  left_join(
    completes,
    by = join_by(specialty),
  ) |>
  left_join(
    incompletes,
    by = join_by(specialty)
  ) |>
  mutate(
    params = purrr::pmap(
      .l = list(
        referrals_data,
        completes_data,
        incompletes_data
      ),
      .f = \(ref, comp, incomp) calibrate_capacity_renege_params(
        referrals = ref,
        completes = comp,
        incompletes = incomp,
        max_months_waited = max_months_waited,
        redistribute_m0_reneges = FALSE,
        full_breakdown = FALSE
      )
    )
  )


all_specialty_projections <- all_specialty_calibration_data |>
  mutate(
    # create referral projections
    ref_projections = purrr::map(
      referrals_data,
      \(x) {
        ref_lm_fit <- lm(referrals ~ period_id, data = x)

        p_val <- broom::tidy(ref_lm_fit) |>
          filter(term == "period_id") |>
          pull(p.value)

        if (p_val > 0.05) {
          ref_start <- mean(x$referrals)
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

        ref_projections[ref_projections < 0] <- 0
        return(ref_projections)
      }
    ),
    # create capacity start value
    start_capacity = purrr::map(
      completes_data,
      \(x) x |>
        summarise(val = sum(treatments),
                  .by = period_id) |>
        filter(period_id == max(period_id)) |>
        pull(val)
    ),
    cap_change = purrr::pmap(
      .l = list(
        start_capacity,
        ref_projections,
        incompletes_data,
        params
      ),
      .f = \(start_cap, ref, incomp, param) {
        optimise_capacity(
          t_1_capacity = start_cap,
          referrals_projections = ref,
          incomplete_pathways = incomp |>
            filter(period_id == max(period_id)) |>
            select(months_waited_id, incompletes),
          renege_capacity_params = param,
          target = "8%",
          target_bin = 4,
          tolerance = 0.001,
          capacity_profile = "flat",
          max_iterations = 35
        )
      }
    ),
    status = names(unlist(cap_change)),
    annual_linear_uplift = as.numeric(cap_change),
    future_performance = purrr::pmap(
      .l = list(
        start_capacity,
        cap_change,
        ref_projections,
        incompletes_data,
        params
      ),
      .f = \(start_capacity, cap_change, ref_projections, incompletes, params) {
        apply_params_to_projections(
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
      }
    )
  )


# charts

treatment_function_codes <- c(
  "(:?C_|[INA]P)?100" = "General Surgery",
  "(:?C_|[INA]P)?101" = "Urology",
  "(:?C_|[INA]P)?110" = "Trauma and Orthopaedic",
  "(:?C_|[INA]P)?120" = "Ear Nose and Throat",
  "(:?C_|[INA]P)?130" = "Ophthalmology",
  "(:?C_|[INA]P)?140" = "Oral Surgery",
  "(:?C_|[INA]P)?150" ="Neurosurgical",
  "(:?C_|[INA]P)?160" = "Plastic Surgery",
  "(:?C_|[INA]P)?170" = "Cardiothoracic Surgery",
  "C_300" = "General Internal Medicine",
  "(:?C_|[INA]P)?301" = "Gastroenterology",
  "(:?C_|[INA]P)?320" = "Cardiology",
  "(:?C_|[INA]P)?330" = "Dermatology",
  "(:?C_|[INA]P)?340" = "Respiratory Medicine",
  "(:?C_|[INA]P)?400" = "Neurology",
  "(:?C_|[INA]P)?410" = "Rheumatology",
  "(:?C_|[INA]P)?430" = "Elderly Medicine",
  "(:?C_|[INA]P)?502" = "Gynaecology",
  "X0[1-6]" = "Other",
  "C_999" = "Total"
)

reorder_vector <- function(x, vals) {
  first_part <- x[!(x %in% vals)]

  reordered <- c(first_part, vals)

  return(reordered)
}

wl_specialty_chart_data <- all_specialty_projections |>
  mutate(
    specialty = stringr::str_replace_all(
      specialty,
      treatment_function_codes
    ),
    specialty = factor(
      specialty,
      levels = reorder_vector(
        sort(unique(specialty)),
        c("Other", "Total")
      )
    )
  ) |>
  select(
    specialty, future_performance
  ) |>
  tidyr::unnest(future_performance)

wl_specialty_chart <- wl_specialty_chart_data |>
  summarise(
    wl = sum(incompletes),
    .by = c(
      specialty, period
    )
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
    vjust = -1,
    hjust = 1
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
  ) +
  facet_wrap(
    facets = vars(specialty),
    scales = "free_y"
  )

ggsave(
  filename = "outputs/wl_specialty_chart.png",
  plot = wl_specialty_chart,
  width = 12,
  height = 8,
  units = "in",
  bg = "white"
)



performance_specialty_chart <- wl_specialty_chart_data |>
  mutate(
    perf = case_when(
      months_waited_id < 4 ~ "Below",
      .default = "Above"
    )
  ) |>
  summarise(
    incompletes = sum(incompletes),
    .by = c(
      specialty, period, perf
    )
  ) |>
  mutate(
    prop = incompletes / sum(incompletes),
    .by = c(specialty, period)
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
    data = . %>% filter(period == max(period), prop != 0),
    aes(label = scales::label_percent(accuracy = 0.1)(prop)),
    vjust = 1.5,
    hjust = 1
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    title = "4 month performance based on flat referrals and flat capacity",
    subtitle = "92% performance is achieved in December 2034",
    y = "Proportion of people on waiting list whose clock stop is under 4 months",
    x = ""
  ) +
  facet_wrap(
    facets = vars(specialty)
  )

ggsave(
  filename = "outputs/performance_specialty_chart.png",
  plot = performance_specialty_chart,
  width = 12,
  height = 8,
  units = "in",
  bg = "white"
)
