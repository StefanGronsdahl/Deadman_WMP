## calculate low flow statistics on 08LF027
### fasstr frequency analysis

rm(list = ls())

# load packages
library(tidyhydat)
library(ggplot2)
library(dplyr)
library(fasstr)
library(lubridate)

# load data
Q = hy_daily_flows(station_number = "08LF027") %>% 
  filter(Date >= "1995-01-01")


Q30d = Q %>%
  add_rolling_means(roll_days = 30, roll_align = "center") %>%
  add_date_variables(water_year_start = 10) %>%
  group_by(WaterYear) %>%
  slice_min(Q30Day, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(WaterYear, Date, Q30 = Q30Day)


# plot timeseries of annual low flows #
ggplot(Q30d, aes(x = WaterYear, y = Q30)) +
  geom_hline(yintercept = 0.462, colour = "#1a9850", linewidth = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.231, colour = "#fdae61", linewidth = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.116, colour = "#d73027", linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = -Inf, y = 0.462, label = "20% MAD",
           hjust = -0.1, vjust = -0.5, size = 3, colour = "#1a9850") +
  annotate("text", x = -Inf, y = 0.231, label = "10% MAD",
           hjust = -0.1, vjust = -0.5, size = 3, colour = "#fdae61") +
  annotate("text", x = -Inf, y = 0.116, label = "5% MAD",
           hjust = -0.1, vjust = -0.5, size = 3, colour = "#d73027") +
  geom_point(colour = "#2c7fb8", size = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Water Year",
    y = expression("30-day Minimum Flow ("*m^3*s^{-1}*")")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("Figures/30Day_Timeseries.png", width = 8, height = 5, dpi = 300)


### frequency analysis
freq_input = Q30d %>%
  select(WaterYear, Q30) %>%
  mutate(Measure = "30-Day")

freq = compute_frequency_analysis(
  data             = freq_input,
  events           = WaterYear,
  values           = Q30,
  measures         = Measure,
  use_max          = FALSE,                      # low-flow minimums
  fit_distr        = "PIII",
  fit_distr_method = "MOM",
  fit_quantiles    = c(0.5, 0.2, 0.1, 0.05, 0.02),
  prob_plot_position = "weibull"
)

# Table of fitted quantiles (Distribution, Probability, Return.Period, 30-Day)
freq$Freq_Fitted_Quantiles

# Figure: fitted curve plotted against the Weibull plotting-position points
freq$Freq_Plot

