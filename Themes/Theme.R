# theme.R

my_theme <- theme(
  plot.background  = element_rect(fill = "#ffffff", color = NA),
  panel.background = element_rect(fill = "#f7f7f7", color = NA),
  plot.margin      = margin(12, 12, 12, 12, "pt"),
  plot.title       = element_text(size = 15, color = "#1a1a1a", family = "sans", hjust = 0.5),
  axis.text        = element_text(size = 14, color = "#1a1a1a", family = "sans"),
  axis.title       = element_text(size = 15, color = "#1a1a1a", family = "sans"),
  legend.text      = element_text(size = 13, color = "#1a1a1a", family = "sans"),
  panel.grid.major = element_line(color = "#e0e0e0", linewidth = 1.3),
  panel.grid.minor = element_line(color = "#f0f0f0", linewidth = 0.7),
  axis.line        = element_line(color = "#888888", linewidth = 2.0),
  axis.ticks       = element_line(color = "#888888"),
  legend.position  = "bottom"
)

my_colors <- c("#4e79a7", "#f28e2b", "#59a14f")