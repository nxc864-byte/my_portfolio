################################################################################
# Assignment 3: Visualizing Volcanic Eruption Data
################################################################################
#
# Nayonika Choudhury
# nxc864@miami.edu
# 11/15/2026
#
# This script loads cleaned historical volcanic eruption data (1500–2026), and 
# exports two aligned figures to results/img/.
#
################################################################################


# SET UP

# Load packages
library(tidyverse)    
library(scales)       
library(cowplot)       
library(sf)            
library(rnaturalearth) 
library(ggspatial)     

# Load the data 

volcanoes <- read_rds("data/processed/clean_volcanoes.rds")
world_sf <- ne_countries(scale = "medium", returnclass = "sf")


# DATA PROCESSING

volcanoes_vis <- volcanoes |>
  mutate(
    volcano_type_clean = str_to_sentence(volcano_type_clean),
    vei_category = case_when(
      vei_clean <= 2 ~ "Low explosivity (VEI 0–2)",
      vei_clean <= 4 ~ "Moderate explosivity (VEI 3–4)",
      vei_clean >= 5 ~ "Catastrophic (VEI 5+)"
    ),
    vei_category = fct_relevel(
      vei_category,
      "Low explosivity (VEI 0–2)",
      "Moderate explosivity (VEI 3–4)",
      "Catastrophic (VEI 5+)"
    )
  )

# Summarizing cumulative mortality and % share by volcano morphology

morphology_summary <- volcanoes_vis |>
  filter(deaths_clean > 0, volcano_type_clean != "Unknown") |>
  group_by(volcano_type_clean) |>
  summarise(
    total_deaths = sum(deaths_clean),
    mean_vei = mean(vei_clean, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct_deaths = total_deaths / sum(total_deaths),
    volcano_type_clean = fct_reorder(volcano_type_clean, total_deaths)
  )

# Building sf spatial point layer for explosive eruptions (VEI 3+)

volcanoes_sf <- volcanoes_vis |>
  filter(!is.na(longitude), !is.na(latitude), vei_clean >= 3) |>
  mutate(
    volcano_type_clean = fct_relevel(
      volcano_type_clean,
      "Stratovolcano", "Caldera", "Complex volcano",
      "Shield volcano", "Other"
    )
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# DATA VISUALIZATION

# Figure 1: Aligned multi-panel statistical analysis 

theme_stat_panel <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 12, color = "gray10"),
    plot.subtitle = element_text(size = 9.5, color = "gray35", margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "gray45", hjust = 1, margin = margin(t = 8)),
    axis.title = element_text(face = "bold", size = 10, color = "gray15"),
    axis.text = element_text(size = 9, color = "gray25"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9.5),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 16, b = 8, l = 12)
  )

# Panel (A): Violin density plot + jittered points 

p1 <- volcanoes_vis |>
  filter(deaths_clean > 0) |>
  ggplot(aes(x = factor(vei_clean), y = deaths_clean, fill = vei_category)) +
  geom_violin(
    data = \(d) d |> add_count(vei_clean) |> filter(n >= 2),
    alpha = 0.35,
    color = NA,
    trim = TRUE,
    scale = "width"
  ) +
  geom_jitter(
    aes(color = vei_category),
    position = position_jitter(width = 0.14, seed = 42),
    alpha = 0.55,
    size = 1.6,
    show.legend = FALSE
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.14,
    linewidth = 0.7,
    color = "black",
    na.rm = TRUE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3,
    stroke = 0.8,
    color = "black",
    show.legend = FALSE
  ) +
  scale_y_log10(
    labels = label_comma(),
    breaks = 10^(0:5)
  ) +
  scale_fill_viridis_d(
    name = "Explosivity category",
    option = "inferno",
    begin = 0.25,
    end = 0.82
  ) +
  scale_color_viridis_d(
    option = "inferno",
    begin = 0.25,
    end = 0.82
  ) +
  labs(
    title = "A) Distribution and mean severity of fatalities by VEI level",
    subtitle = "Violin density, individual fatal eruptions, and group mean ± SE on a log scale (1500–2026)",
    x = "Volcanic Explosivity Index (VEI) scale",
    y = "Recorded fatalities"
  ) +
  theme_stat_panel +
  theme(panel.grid.major.x = element_blank())

# Panel (B): Lollipop chart (cumulative deaths)

p2 <- morphology_summary |>
  ggplot(aes(x = total_deaths, y = volcano_type_clean)) +
  geom_segment(
    aes(x = 0, xend = total_deaths, y = volcano_type_clean, yend = volcano_type_clean),
    color = "gray65",
    linewidth = 1.1
  ) +
  geom_point(
    aes(fill = total_deaths),
    shape = 21,
    size = 5,
    color = "black",
    stroke = 0.8,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = paste0(comma(total_deaths), " (", percent(pct_deaths, accuracy = 0.1), ")")),
    hjust = -0.18,
    size = 3.3,
    fontface = "bold",
    color = "gray15"
  ) +
  scale_x_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.28))
  ) +
  scale_fill_viridis_c(option = "inferno", begin = 0.25, end = 0.85) +
  labs(
    title = "B) Cumulative human mortality by volcano morphology",
    subtitle = "Total deaths and share of global eruption fatalities across volcano morphologies (1500–2026)",
    x = "Total cumulative fatalities",
    y = "Volcano morphology",
    caption = "Data source: NOAA NCEI Significant Volcanic Eruptions Database"
  ) +
  theme_stat_panel +
  theme(panel.grid.major.y = element_blank())


fig1_statistical <- plot_grid(
  p1,
  p2,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1.2, 0.95)
)


# Figure 2: Multi-panel cartographic map 

volcano_palette <- c(
  "Stratovolcano"   = "#B2182B",
  "Caldera"         = "#E08214",
  "Complex volcano" = "#FDB863",
  "Shield volcano"  = "#2166AC",
  "Other"           = "#542788"
)

zoom_box <- tibble(
  xmin = 85, xmax = 168,
  ymin = -13, ymax = 19
)

# Top 3 deadliest global eruptions

top_global_labels <- volcanoes_sf |>
  arrange(desc(deaths_clean)) |>
  distinct(name, .keep_all = TRUE) |>
  slice_head(n = 3) |>
  mutate(
    clean_label = case_when(
      str_detect(name, "Pelée|Pelee") ~ "Mont Pelée (Martinique)\n28,000 deaths",
      str_detect(name, "Ruiz")        ~ "Nevado del Ruiz (Colombia)\n23,080 deaths",
      str_detect(name, "Tambora")     ~ "Mount Tambora (Indonesia)\n11,000 deaths",
      TRUE ~ paste0(str_to_sentence(name), "\n", comma(deaths_clean), " deaths")
    ),
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    label_lon = case_when(
      str_detect(name, "Pelée|Pelee") ~ -25,
      str_detect(name, "Ruiz")        ~ -115,
      str_detect(name, "Tambora")     ~ 66
    ),
    label_lat = case_when(
      str_detect(name, "Pelée|Pelee") ~ 26,
      str_detect(name, "Ruiz")        ~ -9,
      str_detect(name, "Tambora")     ~ -29
    )
  )

# Top 3 deadliest Indo-Pacific eruptions

top_regional_labels <- volcanoes_sf |>
  filter(
    st_coordinates(geometry)[, 1] >= 85,
    st_coordinates(geometry)[, 1] <= 168,
    st_coordinates(geometry)[, 2] >= -13,
    st_coordinates(geometry)[, 2] <= 19
  ) |>
  arrange(desc(deaths_clean)) |>
  distinct(name, .keep_all = TRUE) |>
  slice_head(n = 3) |>
  mutate(
    clean_label = paste0(str_to_sentence(name), " (Indonesia)\n", comma(deaths_clean), " deaths"),
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2],
    label_lon = c(131, 113.5, 134),
    label_lat = c(-10.5, 11.5, 11.5)
  )

theme_volcano_map <- theme_bw(base_size = 10.5) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 11.5, color = "gray10"),
    plot.subtitle = element_text(size = 9, color = "gray35", margin = margin(b = 6)),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 8)),
    axis.title = element_text(face = "bold", size = 9.5, color = "gray15"),
    axis.text = element_text(size = 8.5, color = "gray25"),
    panel.background = element_rect(fill = "#EDF3F8"),
    panel.grid.major = element_line(color = "#D4E0EB", linewidth = 0.25),
    panel.border = element_rect(color = "gray40", linewidth = 0.6),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    legend.background = element_rect(fill = "white", color = "gray75", linewidth = 0.35),
    legend.key = element_rect(fill = "white", color = NA),
    legend.title = element_text(face = "bold", size = 9, color = "gray15"),
    legend.text = element_text(size = 8.5, color = "gray20"),
    plot.margin = margin(t = 8, r = 14, b = 6, l = 12)
  )

# Panel (A): Global distribution

map_global <- ggplot() +
  geom_sf(
    data = world_sf,
    fill = "#E6E2D8",
    color = "#B8B2A6",
    linewidth = 0.2
  ) +
  geom_rect(
    data = zoom_box,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      linetype = "Indo-Pacific hotspot (Panel B)"
    ),
    fill = NA,
    color = "#B2182B",
    linewidth = 0.65
  ) +
  geom_sf(
    data = volcanoes_sf |> arrange(desc(deaths_clean)),
    aes(size = deaths_clean, fill = volcano_type_clean),
    shape = 21,
    color = "white",
    stroke = 0.4,
    alpha = 0.88,
    show.legend = FALSE
  ) +
  geom_sf(
    data = world_sf,
    fill = NA,
    color = "gray35",
    linewidth = 0.22
  ) +
  geom_point(
    data = top_global_labels,
    aes(x = lon, y = lat),
    color = "white",
    size = 0.9
  ) +
  geom_segment(
    data = top_global_labels,
    aes(x = lon, y = lat, xend = label_lon, yend = label_lat),
    color = "gray25",
    linewidth = 0.4
  ) +
  geom_label(
    data = top_global_labels,
    aes(x = label_lon, y = label_lat, label = clean_label),
    size = 2.5,
    fontface = "bold",
    lineheight = 0.95,
    fill = alpha("white", 0.95),
    color = "gray10",
    label.size = 0.3,
    label.padding = unit(0.2, "lines")
  ) +
  scale_size_area(
    max_size = 10,
    breaks = c(10, 500, 5000, 25000)
  ) +
  scale_fill_manual(
    values = volcano_palette
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Indo-Pacific hotspot (Panel B)" = "dashed")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(color = "#B2182B", linewidth = 0.7, fill = NA)
    )
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-58, 80), expand = FALSE) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_minimal(text_size = 6.5, line_col = "gray25", text_col = "gray25"),
    height = unit(0.65, "cm"),
    width = unit(0.65, "cm")
  ) +
  labs(
    title = "A) Global distribution of explosive eruptions (VEI 3+, 1500–2026)",
    subtitle = "Explosive events scaled by recorded death toll and colored by primary volcano morphology",
    x = "Longitude (deg)",
    y = "Latitude (deg)"
  ) +
  theme_volcano_map +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.21, 0.14),
    legend.background = element_rect(fill = alpha("white", 0.95), color = "gray75", linewidth = 0.35),
    legend.margin = margin(t = 3, r = 7, b = 3, l = 6),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.45, "cm"),
    legend.text = element_text(size = 8, face = "bold", color = "gray15")
  )

# Panel (B): Indo-Pacific hotspot zoom

map_indo_pacific <- ggplot() +
  geom_sf(
    data = world_sf,
    fill = "#E6E2D8",
    color = "#B8B2A6",
    linewidth = 0.25
  ) +
  geom_sf(
    data = volcanoes_sf |> arrange(desc(deaths_clean)),
    aes(size = deaths_clean, fill = volcano_type_clean),
    shape = 21,
    color = "white",
    stroke = 0.45,
    alpha = 0.9
  ) +
  geom_segment(
    data = top_regional_labels,
    aes(x = lon, y = lat, xend = label_lon, yend = label_lat),
    color = "gray25",
    linewidth = 0.4
  ) +
  geom_label(
    data = top_regional_labels,
    aes(x = label_lon, y = label_lat, label = clean_label),
    size = 2.5,
    fontface = "bold",
    lineheight = 0.95,
    fill = alpha("white", 0.95),
    color = "gray10",
    label.size = 0.3,
    label.padding = unit(0.2, "lines")
  ) +
  scale_size_area(
    name = "Recorded fatalities (deaths)",
    max_size = 10,
    breaks = c(10, 500, 5000, 25000),
    labels = c("10", "500", "5,000", "25,000+")
  ) +
  scale_fill_manual(
    name = "Volcano morphology",
    values = volcano_palette
  ) +
  coord_sf(xlim = c(85, 168), ylim = c(-13, 19), expand = FALSE) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal(text_size = 6.5, line_col = "gray25", text_col = "gray25"),
    height = unit(0.65, "cm"),
    width = unit(0.65, "cm")
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.65
  ) +
  labs(
    title = "B) Indo-Pacific volcanic hotspot (85°E–168°E, 13°S–19°N)",
    subtitle = "Dense concentration of lethal stratovolcanoes across the Sunda Arc, the Philippines, and Papua New Guinea",
    x = "Longitude (deg)",
    y = "Latitude (deg)",
    caption = "Data source: NOAA NCEI Significant Volcanic Eruptions Database & Natural Earth"
  ) +
  theme_volcano_map +
  guides(
    fill = guide_legend(
      title.position = "top",
      nrow = 2,
      override.aes = list(size = 4, color = "black", stroke = 0.4)
    ),
    size = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(fill = "gray45", color = "black", stroke = 0.5)
    )
  )

fig2_spatial_multipanel <- plot_grid(
  map_global,
  map_indo_pacific,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1, 1.25)
)

# EXPORT

# Export Fig. 1

ggsave(
  plot = fig1_statistical,
  filename = "results/img/vei_and_morphology_fatalities.png",
  width = 8.5,
  height = 9.5,
  dpi = 300
)

# Export Fig. 2

ggsave(
  plot = fig2_spatial_multipanel,
  filename = "results/img/spatial_eruptions_by_vei.png",
  width = 9.5,
  height = 10,
  dpi = 300
)