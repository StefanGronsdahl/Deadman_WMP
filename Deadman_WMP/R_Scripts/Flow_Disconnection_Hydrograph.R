
# Deadman WSC flows
# script to create hydrograph for WSC 

library(tidyhydat)
library(ggplot2)
library(dplyr)

start = "2025-10-15"
end = "2025-11-15"

# read data
Q_WSC = available_flows(station_number = "08LF027", 
                            start_date = start,
                            end_date = end)


Q_Snohoosh = read.csv("Data_Inputs/SLR_Hydro_Data/Q_DR-H02.csv", skip = 14) %>%
  mutate(Date = as.Date(Timestamp..UTC.08.00.)) %>% group_by(Date) %>%
  summarize(Q = mean(Value)) %>%
  filter(Date >= start & Date <= end)

Q_DStream = read.csv("Data_Inputs/SLR_Hydro_Data/Q_DR-H01.csv", skip = 14) %>%
  mutate(Date = as.Date(Timestamp..UTC.08.00.)) %>% group_by(Date) %>%
  summarize(Q = mean(Value)) %>%
  filter(Date >= start & Date <= end)


ggplot() + 
  geom_line(data = Q_WSC, aes(x = Date, y = Value, colour = "08LF027"), linewidth = 0.8) +
  geom_line(data = Q_Snohoosh, aes(x = Date, y = Q, colour = "DR-H02"), linewidth = 0.8) +
  #  geom_line(data = Q_DStream, aes(x = Date, y = Q, colour = "DR-H03"), linewidth = 0.8) +
  
  # Horizontal threshold lines
  geom_hline(aes(yintercept = 0.494, colour = "20% LT MAD"), linetype = "dashed", linewidth = 0.7) +
  geom_hline(aes(yintercept = 0.247, colour = "10% LT MAD"), linetype = "dashed", linewidth = 0.7) +
  geom_hline(aes(yintercept = 0.124, colour = "5% LT MAD"),  linetype = "dashed", linewidth = 0.7) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "08LF027"    = "#2196A6",
      "DR-H02"     = "#1B5E6B",
      "20% LT MAD" = "#E8B84B",
      "10% LT MAD" = "#E07B2A",
      "5% LT MAD"  = "#C0392B"
    ),
    breaks = c("08LF027", "DR-H02", "20% LT MAD", "10% LT MAD", "5% LT MAD")
  ) +
  
  # Axis labels
  labs(
    title = "Daily Streamflow – Station 08LF027",
    x = NULL,
    y = expression(Discharge~(m^3/s))
  ) +
  
  # Clean white theme with gridlines
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2),
    axis.text = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.margin = margin(10, 15, 10, 10),
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3),
    legend.margin = margin(4, 6, 4, 6)
  ) +
  
  # Y axis formatting
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.0, 0.0)),
    limits = c(0, 0.5)
  ) +
  scale_x_date(date_labels = "%d-%b", date_breaks = "4 days")

ggsave("Figures/flow_disconnect_hydrograph.png", width = 8, height = 5, dpi = 300)

#### review Kamloops weather data 

install.packages("remotes")
remotes::install_github("ropensci/weathercan")
library(weathercan)

# Download daily data for Kamloops Airport
kamloops <- weather_dl(
  station_ids = 51423,   # Kamloops A station (EC station ID)
  start = "2025-10-01",
  end = "2025-11-30",
  interval = "day"
)

# Pull just date and precip columns
precip <- kamloops %>%
  select(date, total_precip) %>%
  filter(!is.na(total_precip))

# Quick look
print(precip)



