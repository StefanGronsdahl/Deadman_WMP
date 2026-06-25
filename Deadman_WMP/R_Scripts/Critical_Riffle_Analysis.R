#### Script to Conduct Critical Riffle Analysis####
# Stefan Gronsdahl

################
rm(list = ls())

####################
library(tidyhydat)
library(ggplot2)
library(dplyr)

# read riffle data
transects_all = read.csv("Data_Inputs/SFC_Transects/HSITransectData_July2024.csv")

riffles23 = transects_all %>% filter(reach > 1 & habitat.type == "riffle") %>%
  group_by(reach, transect.num) %>%
  summarize(wetted.w = mean(wetted.w))

# read  flow data data
Q_WSC = available_flows(station_number = "08LF027", 
                        start_date = "2024-07-10",
                        end_date = "2024-07-14")

# Survey Q
SurveyQ = mean(Q_WSC$Value)

# compute watershed area correct MAD
# Gorge Creek WA = 67 km2, WSC gauge WA = 882 km2
MAD_Riffle23 = 2.427 * 785 / 822

riffles23$SurveyQ = SurveyQ

# define exponent for at-a-station geometry
b = 0.2

# calculate a value
riffles23$a = riffles23$wetted.w / (SurveyQ^b)

# estimate wetted width at 100% MAD
riffles23$ww_100MAD = riffles23$a * MAD_Riffle23^b

# calculate wetted width equivalent to 60% of that at 100% MAD
riffles23$ww60perc = riffles23$ww_100MAD * 0.6

# calculate corresponding flow value
riffles23$Q60perc_ww = (riffles23$ww60perc / riffles23$a)^(1/b)

# flow value as % MAD
riffles23$Q60perc_ww_MAD = riffles23$Q60perc_ww / MAD_Riffle23

#### passage depth for adults ####
##################################

# base depth data (same selection as before)
depth_base = transects_all %>%
  filter(reach > 1 & habitat.type == "riffle") %>%
  select(reach, transect.num, depth, distance.m, wetted.w)

# set exponent value
f = 0.4

# at-a-station depth coefficient — solved from the survey, flow-independent, once
depth_base$a = depth_base$depth / SurveyQ^f

# wetted width at 100% MAD (constant)
w_100MAD = riffles23 %>% select(reach, transect.num, ww_100MAD)

# grids to iterate over
flow_fracs   = seq(0.10, 0.20, by = 0.025)     # 10, 12.5, 15, 17.5, 20% MAD
d_thresholds = c(0.15, 0.18, 0.21, 0.24)

# container
depth_pass_all = data.frame()

for (qf in flow_fracs) {
  testQ = MAD_Riffle23 * qf
  td = depth_base %>% mutate(test_depth = a * testQ^f)   # projection moves with flow
  
  for (d_thresh in d_thresholds) {
    tmp = td %>%
      mutate(passable = if_else(test_depth - d_thresh >= 0, 1, 0)) %>%
      group_by(reach, transect.num) %>%
      summarise(passable_width = sum(passable), .groups = "drop") %>%
      left_join(w_100MAD, by = c("reach", "transect.num")) %>%
      mutate(perc      = passable_width / ww_100MAD,
             d_thresh  = d_thresh,
             flow_frac = qf)
    depth_pass_all = bind_rows(depth_pass_all, tmp)
  }
}

# ordered flow label for plotting/legend
depth_pass_all = depth_pass_all %>%
  mutate(flow_label = factor(paste0(flow_frac * 100, "% MAD"),
                             levels = paste0(sort(unique(flow_frac)) * 100, "% MAD")))


# summary table
depth_pass_summary = depth_pass_all %>%
  group_by(flow_frac, d_thresh) %>%
  summarise(mean_perc = mean(perc), median_perc = median(perc), .groups = "drop")


# plot
ggplot(depth_pass_all, aes(x = factor(d_thresh), y = perc, fill = flow_label)) +
  geom_boxplot(width = 0.7, outlier.shape = NA,
               position = position_dodge(0.8),
               colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0.25, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 0.55, y = 0.25, label = "25%",
           hjust = 0, vjust = -0.5, size = 3, colour = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  scale_fill_viridis_d(name = "Discharge") +
  labs(
    x = "Depth Threshold (m)",
    y = "Passable Proportion of Wetted Width"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave("Figures/CRA_box_whiskers.png", width = 8, height = 5, dpi = 300)


######################################################
