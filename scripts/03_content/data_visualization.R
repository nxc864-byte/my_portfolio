################################################################################
# Assignment 2 EVR 628
################################################################################
#
# Nayonika Choudhury
# nxc864@miami.edu
# 09/30/3036
#
# Script 2
#
################################################################################

library(tidyverse)
library(scales)

volcanoes <- read_rds("data/processed/clean_volcanoes.rds")

# Plot 1: VEI vs Fatalities (Log-Scale Boxplot + Jitter)
# REQS: Title, axis labels, caption indicating data source

plot1 <- volcanoes %>%
  filter(deaths_clean > 0) %>%
  ggplot(aes(x = factor(vei_clean), y = deaths_clean, fill = vei_category)) +
  
  geom_boxplot(alpha = 0.6,
               outlier.shape = NA,
               color = "black",
               width = 0.5) +
  
  geom_jitter(width = 0.2,
              alpha = 0.5, 
              size = 1.6,
              shape = 21,
              fill = "gray15",
              color = "black") +
  
  scale_y_log10(
    labels = scales::comma,
    breaks = 10^(0:5)) +
  
  scale_fill_brewer(name = "VEI Category",
                    palette = "YlGnBu") +
  
  labs(
    title = "Fatalities by Volcanic Explosivity Index (VEI)",
    subtitle = "Logarithmic Scale of Recorded Deaths for Eruptions with >0 Fatalities (1500–2026)",
    x = "Volcanic Explosivity Index (VEI)",
    y = "Recorded Fatalities",
    caption = "Data Source: NOAA NCEI Significant Volcanic Eruptions Database"
  ) +
  
  theme_minimal(
    base_family = "Times New Roman",
    base_size = 12) +
  
  theme( 
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30",
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0.5, color = "gray40",
                                margin = margin(t = 10)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.box.margin = margin(t = 10),
    panel.grid.minor = element_blank()) + 
  
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave(
  filename = "data/output/plot1_vei_vs_fatalities.png", 
  plot = plot1, 
  width = 8, 
  height = 6, 
  dpi = 300)

plot1

# Plot 2: Cumulative Fatalities by Volcano Type (Morphology)
# REQS: Title, axis labels, caption indicating data source 

plot2 <- volcanoes %>%
  filter(deaths_clean > 0, volcano_type_clean != "Unknown") %>%
  group_by(volcano_type_clean) %>%
  summarise(
    total_deaths = sum(deaths_clean),
    eruption_count = n(),
    .groups = "drop"
  ) %>%
  
  ggplot(aes(x = reorder(volcano_type_clean, total_deaths), y = total_deaths, fill = volcano_type_clean)) +
  
  geom_col(show.legend = FALSE, width = 0.6, color = "black") +
  
 # IN ORDER TO LABEL BARS
  geom_text(
    aes(label = scales::comma(total_deaths)),
    hjust = -0.15,
    family = "Times New Roman",
    size = 3.5,
    fontface = "bold") +
  
  coord_flip() +
  
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.20))) +
  
  scale_fill_brewer(palette = "GnBu", direction = -1) +
  
  labs(
    title = "Total Recorded Fatalities Across Major Volcano Types",
    subtitle = "Stratovolcanoes account for the majority of human mortality (1500–2026)",
    x = "Volcano Type (Morphology)",
    y = "Total Cumulative Fatalities",
    caption = "Data Source: NOAA NCEI Significant Volcanic Eruptions Database"
  ) +
  
  theme_minimal(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0.5, color = "gray40", margin = margin(t = 10)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 8), color = "gray25"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()) 

ggsave(
  filename = "data/output/plot2_volcano_type_deaths.png", 
  plot = plot2, 
  width = 8.5, 
  height = 5.5, 
  dpi = 300) 

plot2

# Plot 3: Global Spatial Mapping of Major Explosive Eruptions (VEI > 4)
# REQS: Title, axis labels, caption indicating data source 

world_map <- map_data("world")

map_volcanoes <- volcanoes %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  filter(vei_clean >= 4)

max_fatality_volcano <- map_volcanoes %>%
  filter(deaths_clean == max(deaths_clean, na.rm = TRUE)) %>%
  slice(1)

min_fatality_volcano <- map_volcanoes %>%
  filter(deaths_clean > 0) %>%
  filter(deaths_clean == min(deaths_clean, na.rm = TRUE)) %>%
  slice(1)

plot3 <- ggplot(data = map_volcanoes) +
  
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "#E3EFF6",
    color = "#222222",
    linewidth = 0.2) +
  
  geom_point( 
    data = map_volcanoes,
    aes(
      x = longitude,
      y = latitude,
      size = deaths_clean + 1,  # +1 : 0-death VEI 4 eruptions remain visible
      fill = volcano_type_clean),
    shape = 21,
    color = "black",
    stroke = 0.35,
    alpha = 0.8) + 
  
  geom_text(
    data = max_fatality_volcano,
    aes(
      x = longitude,
      y = latitude,
      label = paste0("Max: ", comma(deaths_clean), " deaths")),
      family = "Times New Roman",
      fontface = "bold",
      size = 2, 
      color = "#8B0000",
      vjust = 0.5,
      hjust = -0.3) + 
  
  geom_text(
    data = min_fatality_volcano,
    aes(
      x = longitude,
      y = latitude,
      label = paste0("Min: ", comma(deaths_clean), " death(s)")), 
    family = "Times New Roman",
    fontface = "bold",
    size = 2,
    color = "#1C39BB",
    vjust = 4,
    hjust = 0.5) + 
  
  scale_size_continuous(
    name = "Fatalities",
    range = c(2.5, 9), 
    breaks = c(1, 100, 1000, 10000),
    labels = c("1", "100", "1,000", "10,000+")) + 
  
  scale_fill_brewer(
    name = "Volcano Morphology",
    palette = "Set1") + 
  
coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  
  labs(
    title = "Global Distribution of Major Explosive Eruptions (VEI 4+)",
    subtitle = "Spatial Location Scaled by Fatality Count and Categorized by Volcano Morphology (1500–2026)",
    x = "Longitude",
    y = "Latitude",
    caption = "Data Source: NOAA NCEI Significant Volcanic Eruptions Database") + 
  
  theme_void(base_family = "Times New Roman", base_size = 12) +
  
  theme(
    plot.background = element_rect(fill = "#FDF0ED", color = NA),
    panel.background = element_rect(fill = "#FDF0ED", color = NA),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5,
                                  margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", 
                                     margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0.5, color = "gray40",
                                    margin = margin(t = 10)),
        
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(1.2, "cm"),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.background = element_rect(fill = "#FDF0ED", color = NA),
    legend.key = element_rect(fill = "#FDF0ED", color = NA),
    legend.margin = margin(t = 5, b = 5)) +
  
  guides(
    fill = guide_legend(title.position = "top", title.hjust = 0.5, 
                        override.aes = list(size = 4)),
    size = guide_legend(title.position = "top", title.hjust = 0.5)) 

ggsave(
  filename = "results/img/plot3_spatial_impact_map.png",
  plot = plot3,
  width = 10, 
  height = 6, 
  dpi = 300,
  bg = "#FDF0ED")
  
plot3


